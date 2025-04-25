module Data.Jianpu.Syntax where

import Data.Jianpu.Types

type DraftMusic = [DraftVoice]

type DraftVoice = ([Lexeme], [[Maybe Syllable]])

data Lexeme
    = LEvent Event
    | ColonSingleBarColon
    | SingleBarColon
    | ColonSingleBar
    | SingleBar
    | DoubleBar
    | TripleBar
    | Tag0 String [Argument]
    | Tag1 String [Argument]
    | TagStart String (Maybe Int) [Argument]
    | TagEnd (Maybe String) (Maybe Int)
    deriving (Show)

data Argument
    = AInt Int
    | AStr String
    deriving (Show)
