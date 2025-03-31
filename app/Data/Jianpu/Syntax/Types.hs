module Data.Jianpu.Syntax.Types where

import Data.Jianpu.Types

newtype Music = Music [Voice] deriving (Show)
data Voice = Voice
    { entities :: [Entity]
    , lyricsLines :: [[Maybe Syllable]]
    }
    deriving (Show)

data Entity
    = Event Event
    | BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    | BarLine
    | Tag0 String [Argument]
    | Tag1 String [Argument]
    | TagStart String (Maybe Int) [Argument]
    | TagEnd (Maybe String) (Maybe Int)
    deriving (Show)

data Argument
    = Int Int
    | String String
    deriving (Show)
