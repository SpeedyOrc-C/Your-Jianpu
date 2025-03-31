module Data.Jianpu.Syntax.Parser where

import Text.Parsec

import Control.Applicative (asum, some)
import Control.Monad
import Data.Jianpu.Syntax.Types
import Data.Jianpu.Types
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE

type Parser a = Parsec String () a

pRepeater4 :: Parser Event
pRepeater4 = Repeater4 <$ string "-"

pInt :: Parser Int
pInt = read <$> some digit

pString :: Parser String
pString = char '"' *> many pInnerChar <* char '"'
  where
    pInnerChar :: Parser Char
    pInnerChar = pEscapedChar <|> pVisibleChar

    pVisibleChar :: Parser Char
    pVisibleChar = noneOf "\\\"\n\r\t"

    pEscapedChar :: Parser Char
    pEscapedChar =
        char '\\'
            *> asum
                [ '\\' <$ char '\\'
                , '"' <$ char '"'
                , fail "只支持转义反斜线 \\ 和双引号 \" 。"
                ]

pIdentifier :: Parser String
pIdentifier = (:) <$> pFirst <*> many (noneOf " !@#$%^&*:;,()<>{}\\\"\n\r\t")
  where
    pFirst = noneOf " !@#$%^&*:;,()<>{}\\\"\n\r\t0123456789"

pMultiBarRest :: Parser Event
pMultiBarRest = do
    _ <- string "=="
    n <- MultiBarRest <$> pInt <|> fail "缺少需要休止的小节数量。"
    _ <- string "=="
    return n

pTimeMultiplier :: Parser TimeMultiplier
pTimeMultiplier = do
    (length -> n) <- many (void $ char '/')
    case n of
        0 -> return Whole
        1 -> return Minim
        2 -> return Crotchet
        3 -> return Quaver
        4 -> return Semiquaver
        _ -> fail "最多只支持到 64 分音符，即最多四条斜线。"

pDot :: Parser Int
pDot = length <$> many (void $ char '*')

pClap :: Parser Sound
pClap = Clap <$ char 'X'

pRest :: Parser Sound
pRest = Rest <$ char '0'

pWhiteKey :: Parser WhiteKey
pWhiteKey =
    asum
        [ K1 <$ char '1'
        , K2 <$ char '2'
        , K3 <$ char '3'
        , K4 <$ char '4'
        , K5 <$ char '5'
        , K6 <$ char '6'
        , K7 <$ char '7'
        ]

pTranspose :: Parser Int
pTranspose = do
    (length -> up) <- many (void $ char '\'')
    if up == 0
        then do
            (length -> down) <- many (void $ char '.')
            return (-down)
        else
            return up

pAccidental :: Parser (Maybe Accidental)
pAccidental =
    asum
        [ Just Sharp <$ char '#'
        , char 'b'
            *> (Just DoubleFlat <$ char 'b' <|> return (Just Flat))
        , Just DoubleSharp <$ char 'x'
        , Just Natural <$ char '='
        , return Nothing
        ]

pPitch :: Parser Pitch
pPitch = do
    accidental <- pAccidental
    whiteKey <- pWhiteKey
    transpose <- pTranspose
    return $ Pitch whiteKey transpose accidental

pWhites :: Parser ()
pWhites = void $ many pWhite
  where
    pWhite = asum $ map char " \t\n\r"

pSingleNote :: Parser (NonEmpty Pitch)
pSingleNote = NE.singleton <$> pPitch

pChord :: Parser (NonEmpty Pitch)
pChord = do
    void $ char '['
    pWhites
    firstPitch <- pPitch <* pWhites
    tailPitches <- some $ pPitch <* pWhites
    void $ char ']'
    return $ firstPitch :| tailPitches

pPitches :: Parser (NonEmpty Pitch)
pPitches = pSingleNote <|> pChord

pNote :: Parser Sound
pNote = Note <$> pPitches <*> pure Nothing

pAction :: Parser Event
pAction = do
    sound <- pNote <|> pRest <|> pClap
    timeMultiplier <- pTimeMultiplier
    dot <- pDot
    return $ Action timeMultiplier dot sound

pEvent :: Parser Entity
pEvent = Event <$> (pRepeater4 <|> pMultiBarRest <|> pAction)

pBarLine :: Parser Entity
pBarLine =
    asum
        [ BeginEndRepeat <$ try (string ":|:")
        , BeginRepeat <$ try (string ":|")
        , EndRepeat <$ try (string "|:")
        , EndSign <$ try (string "|||")
        , DoubleBarLine <$ try (string "||")
        , BarLine <$ try (char '|')
        ]

pArgument :: Parser Argument
pArgument = (Int <$> pInt) <|> (String <$> pString)

pArguments :: Parser [Argument]
pArguments = (start *> args <* end) <|> return []
  where
    start = char '<' *> pWhites
    args = pArgument `sepBy` (pWhites *> char ',' <* pWhites)
    end = pWhites <* char '>'

pTag0 :: Parser Entity
pTag0 = char '^' *> (Tag0 <$> pIdentifier <*> pArguments)

pTag1 :: Parser Entity
pTag1 = char '@' *> (Tag1 <$> pIdentifier <*> pArguments)

pTagId :: Parser (Maybe Int)
pTagId = optionMaybe (char ':' *> pInt)

pTagStart :: Parser Entity
pTagStart = char '\\' *> (TagStart <$> pIdentifier <*> pTagId <*> pArguments) <* pWhites <* char '{'

pTagEnd :: Parser Entity
pTagEnd = char '}' *> (TagEnd <$> optionMaybe pIdentifier <*> pTagId)

pEntity :: Parser Entity
pEntity =
    asum
        [ pEvent
        , pBarLine
        , pTag0
        , pTag1
        , pTagStart
        , pTagEnd
        ]

pVoice :: Parser Voice
pVoice = do
    void $ try $ string "<voice>"
    pWhites
    entities <- pEntities
    pWhites
    lyrics <- (:) <$> pLyrics <*> many (try (pWhites *> pLyrics))
    pWhites
    void $ try $ string "<end>"

    pure $ Voice entities lyrics

pEntities :: Parser [Entity]
pEntities = do
    void $ try $ string "<note>"
    pWhites
    entities <- (:) <$> pEntity <*> many (try (pWhites *> pEntity))
    pWhites
    void $ try $ string "<end>"

    pure entities

pLyrics :: Parser [Maybe Syllable]
pLyrics = do
    void $ try $ string "<lyrics>"
    pWhites
    l <- (:) <$> pMaybeSyllable <*> many (try (pWhites *> pMaybeSyllable))
    pWhites
    void $ try $ string "<end>"

    pure l

pMaybeSyllable :: Parser (Maybe Syllable)
pMaybeSyllable =
    (Nothing <$ char '_')
        <|> (Just <$> (Syllable <$> pSyllablePrefix <*> pSyllableContent <*> pSyllableSuffix))

pSyllablePrefix :: Parser (Maybe String)
pSyllablePrefix =
    optionMaybe . asum . fmap string $
        ["‘", "“"]

pSyllableContent :: Parser String
pSyllableContent = concat <$> ((:) <$> pCore <*> many pTail)
  where
    pCore = some (noneOf "<>\n\r\t ’”,.!?，。！？、")
    pTail = (:) <$> (char '\'' <|> char '’') <*> pCore

pSyllableSuffix :: Parser (Maybe String)
pSyllableSuffix =
    optionMaybe . asum . fmap string $
        ["’", "”", ",", ".", "!", "?", "-", "\\;", ":", "，", "。", "！", "？", "、", "；", "："]

pMusic :: Parser Music
pMusic = Music <$> ((:) <$> pVoice <*> many (try (pWhites *> pVoice)))

run :: Parsec String () a -> String -> Either ParseError a
run p = runParser p () ""

test :: IO ()
test = do
    raw <- readFile "test.yjp"
    print $ run pMusic raw
