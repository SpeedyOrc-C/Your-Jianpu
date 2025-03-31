module Data.Jianpu.Abstract.Error where

data AbstractError
    = InconsistentSyllableCount Int
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
