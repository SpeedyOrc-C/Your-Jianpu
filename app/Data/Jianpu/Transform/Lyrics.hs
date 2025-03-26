module Data.Jianpu.Transform.Lyrics where

import Data.Char
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.List

type Syllables = [String]

{- |
A powerful version of `words` that can handle Chinese characters, full-width
punctuations, and hyphen-separated words (split into syllables).

>>> words' "大白菜鸡毛菜通心菜油麦菜"
["\22823","\30333","\33756","\40481","\27611","\33756","\36890","\24515","\33756","\27833","\40614","\33756"]

>>> words' "小朋友，你是否有很多问号？"
["\23567","\26379","\21451\65292","\20320","\26159","\21542","\26377","\24456","\22810","\38382","\21495\65311"]

>>> words' "I am the storm that is ap-proa-ching"
["I","am","the","storm","that","is","ap-","proa-","ching"]

>>> words' "O-ceans a-part, day af-ter day."
["O-","ceans","a-","part,","day","af-","ter","day."]

>>> words' "Hun-gry for my 气"
["Hun-","gry","for","my","\27668"]

>>> words' "我却靠在墙壁背我的 A B C"
["\25105","\21364","\38752","\22312","\22681","\22721","\32972","\25105","\30340","A","B","C"]
-}
words' :: String -> Syllables
words' "" = []
words' ('_' : '_' : cs) = "_" : words' cs
words' ('_' : cs) = "" : words' cs
words' (c : cs)
    | isSpace c =
        words' cs
    | isFullWidthPunctuation c || isPunctuation c =
        case words' cs of
            [] -> [[c]]
            s : ss -> (c : s) : ss
    | isCJKV c =
        let (ps, cs') = span isFullWidthPunctuation cs
         in (c : ps) : words' cs'
    | isAlpha c =
        let (wt, cs') = span isAlpha cs
         in case cs' of
                '-' : cs'' ->
                    (c : wt ++ "-") : words' cs''
                '\'' : cs'' ->
                    let (w', cs''') = span isAlpha cs''
                        (ps, cs'''') = span isPunctuation cs'''
                     in (c : wt ++ "'" ++ w' ++ ps) : words' cs''''
                _ ->
                    let (ps, cs'') = span isPunctuation cs'
                     in (c : wt ++ ps) : words' cs''
    | otherwise = error $ "words': unexpected character " ++ show c
  where
    isCJKV :: Char -> Bool
    isCJKV c' = '\x4E00' <= c' && c' <= '\x9FFF'

    isFullWidthPunctuation :: Char -> Bool
    isFullWidthPunctuation = (`elem` "，。！？：；‘’“”")

type SyllableCounts = [Int]

data LyricsExpansionError = ErrorLE
    { desiredSyllablesCount :: Int
    , actualSyllablesCount :: [Int]
    }
    deriving (Show)

{- |
This function finds out which character or word in a line of lyrics goes to
which note. This will automatically skip elements that are not a note like
bar-lines, rests, and tags. If there are more than one verse, the lyrics lines
will be transposed like a matrix as well.

### Skipping non-note elements

@
\lyrics<"The Quick Brown Fox"> { 1 2 | 3 4 } |||
@

expands to

@
\text-below<"The">   { 1 } \text-below<"Quick"> { 2 } |
\text-below<"Brown"> { 3 } \text-below<"Fox">   { 4 } |||
@

### Multiple verses

@
\\lyrics<"The Quick Brown Fox", "Lazy Dog Over There"> { 1 2 | 3 4 } |||
@

expands to

@
\text-below<"The",   "Lazy"> { 1 } \text-below<"Quick", "Dog">   { 2 }   |
\text-below<"Brown", "Over"> { 3 } \text-below<"Fox",   "There"> { 4 }   |||
@
-}
expandLyrics :: Voice -> Either LyricsExpansionError Voice
expandLyrics Voice{entities, tagSpans} =
    case eitherNewSpans of
        Left err ->
            Left err
        Right (IM.fromAscList -> tagSpansToBeAdded) ->
            Right Voice{entities, tagSpans = tagSpans' <> tagSpansToBeAdded}
  where
    (tagSpans', lyricsTagSpans) =
        flip IM.mapEither tagSpans $ \case
            Lyrics lyrics -> Right lyrics
            s -> Left s

    syllablesTagSpansMaybe = map words' <$> lyricsTagSpans

    indexedItems = zip [0 ..] entities

    eitherNewSpans = sequence $ do
        (lyricsInterval, syllablesList) <- IM.toAscList syllablesTagSpansMaybe

        let (left, right) = case lyricsInterval of
                IM.ClosedInterval a b -> (a, b)
                IM.OpenInterval a b -> (a + 1, b - 1)
                IM.IntervalCO a b -> (a, b - 1)
                IM.IntervalOC a b -> (a + 1, b)

        let syllableFriendlyItemsIndices =
                [ index
                | (index, entity) <- take (right - left + 1) (drop left indexedItems)
                , case entity of
                    Event{event = Repeater4} -> True
                    Event{event = (Action{sound = Clap})} -> True
                    Event{event = (Action{sound = Note{}})} -> True
                    _ -> False
                ]

        let desiredSyllablesCount = length syllableFriendlyItemsIndices

        let actualSyllablesCount = length <$> syllablesList

        if any ((/= desiredSyllablesCount) . length) syllablesList
            then
                return $ Left ErrorLE{desiredSyllablesCount, actualSyllablesCount}
            else do
                let syllablesListByTime = transpose syllablesList
                let rawTagSpans = zip syllableFriendlyItemsIndices syllablesListByTime
                let rawTagSpansNoEmptyOnes = filter (not . all null . snd) rawTagSpans

                (index, syllables) <- rawTagSpansNoEmptyOnes

                let interval = IM.ClosedInterval index index
                let textBelow = TextBelow syllables

                return $ Right (interval, textBelow)
