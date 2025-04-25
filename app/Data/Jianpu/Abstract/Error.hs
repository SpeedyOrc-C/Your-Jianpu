module Data.Jianpu.Abstract.Error where
import Text.Parsec (ParseError)

type HasError = Either [AbstractError]

data AbstractError
    = MarkupSyntaxError ParseError
    | InconsistentSyllableCount Int
    | ErrorArgsTimeSignature
    | ErrorArgsFermata
    | ErrorArgsBeam
    | ErrorArgsTie
    | ErrorArgsSlur
    | ErrorArgsTuplet
    | ErrorArgsLyrics
    | UnknownTag String
    | TagMismatch
    deriving (Show)
