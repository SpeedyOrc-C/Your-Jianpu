module Data.Jianpu.Syntax.Parser where

import Text.Parsec

import Control.Applicative (asum, some)
import Control.Monad
import Data.Jianpu.Abstract.Error (AbstractError (MarkupSyntaxError), HasError)
import Data.Jianpu.Syntax
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
pWhites = void $ many (pComment <|> pWhite)

pWhites1 :: Parser ()
pWhites1 = void $ some pWhite

pWhite :: Parser ()
pWhite = void $ asum (map char " \t\n\r")

pComment :: Parser ()
pComment = void $ do
    void $ try (string "--")
    void $ many (noneOf "\n\r")
    void $ some (asum (map char "\n\r"))

pSingleNote :: Parser (NonEmpty Pitch)
pSingleNote = NE.singleton <$> pPitch

pPitches :: Parser (NonEmpty Pitch)
pPitches = do
    pitches <- pPitch `sepBy1` char '+'
    case pitches of
        [] -> error "This is not possible."
        p : ps -> return $ p :| ps

pNote :: Parser Sound
pNote = Note <$> pPitches <*> pure Nothing

pAction :: Parser Event
pAction = do
    sound <- pNote <|> pRest <|> pClap
    timeMultiplier <- pTimeMultiplier
    dot <- pDot
    return $ Action timeMultiplier dot sound

pEvent :: Parser Lexeme
pEvent = LEvent <$> (pRepeater4 <|> try pMultiBarRest <|> pAction)

pBarLine :: Parser Lexeme
pBarLine =
    asum
        [ ColonSingleBarColon <$ try (string ":|:")
        , SingleBarColon <$ try (string ":|")
        , ColonSingleBar <$ try (string "|:")
        , TripleBar <$ try (string "|||")
        , DoubleBar <$ try (string "||")
        , SingleBar <$ try (char '|')
        ]

pArgument :: Parser Argument
pArgument = (AInt <$> pInt) <|> (AStr <$> pString)

pArguments :: Parser [Argument]
pArguments = (start *> args <* end) <|> return []
  where
    start = char '<' *> pWhites
    args = pArgument `sepBy` (pWhites *> char ',' <* pWhites)
    end = pWhites <* char '>'

pTag0 :: Parser Lexeme
pTag0 = char '^' *> (Tag0 <$> pIdentifier <*> pArguments)

pTag1 :: Parser Lexeme
pTag1 = char '@' *> (Tag1 <$> pIdentifier <*> pArguments)

pTagId :: Parser (Maybe Int)
pTagId = optionMaybe (char ':' *> pInt)

pTagStart :: Parser Lexeme
pTagStart = char '\\' *> (TagStart <$> pIdentifier <*> pTagId <*> pArguments) <* pWhites <* char '{'

pTagEnd :: Parser Lexeme
pTagEnd = char '}' *> (TagEnd <$> optionMaybe pIdentifier <*> pTagId)

pLexeme :: Parser Lexeme
pLexeme =
    asum
        [ pEvent
        , pBarLine
        , pTag0
        , pTag1
        , pTagStart
        , pTagEnd
        ]

pLexemes :: Parser [Lexeme]
pLexemes = do
    void $ string "note"
    pWhites
    void $ char '['
    pWhites
    entities <- many (try pLexeme <* pWhites)
    pWhites
    void $ char ']'

    pure entities

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
    pCore = some (noneOf "<>[]{}\n\r\t ’”,.!?，。！？、")
    pTail = (:) <$> oneOf "'’" <*> pCore

pSyllableSuffix :: Parser (Maybe String)
pSyllableSuffix =
    optionMaybe . asum . fmap string $
        ["’", "”", ",", ".", "!", "?", "-", "\\;", ":", "，", "。", "！", "？", "、", "；", "："]

pLyrics :: Parser [Maybe Syllable]
pLyrics = do
    void $ string "lyrics"
    pWhites
    void $ char '['
    pWhites
    l <- many (try pMaybeSyllable <* pWhites)
    pWhites
    void $ char ']'

    pure l

pVoice :: Parser ([Lexeme], [[Maybe Syllable]])
pVoice = do
    void $ string "voice"
    pWhites
    void $ char '['
    pWhites
    entities <- pLexemes
    pWhites
    lyrics <- many (try pLyrics <* pWhites)
    void $ char ']'

    pure (entities, lyrics)

pMusic :: Parser [([Lexeme], [[Maybe Syllable]])]
pMusic = many (try pVoice <* pWhites)

pFile :: Parser [([Lexeme], [[Maybe Syllable]])]
pFile = pWhites *> pMusic <* eof

markupToDraft :: FilePath -> String -> HasError [([Lexeme], [[Maybe Syllable]])]
markupToDraft inputPath markup =
    case runParser pFile () inputPath markup of
        Left err -> Left [MarkupSyntaxError err]
        Right result -> Right result

run :: Parsec String () a -> String -> Either ParseError a
run p = runParser p () ""

test :: IO ()
test = do
    raw <- readFile "test.yjp"
    print $ run pFile raw
