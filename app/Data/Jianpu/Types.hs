module Data.Jianpu.Types where

import Data.List.NonEmpty (NonEmpty)

data Event
    = Repeater4
    | MultiBarRest Int
    | TimedEvent
        { timeMultiplier :: TimeMultiplier
        , dot :: Int
        , sound :: Sound
        }
    deriving (Show, Eq)

data TimeMultiplier = Whole | Minim | Crotchet | Quaver | Semiquaver
    deriving (Show, Eq)

data Sound
    = Note
        { pitches :: NonEmpty Pitch
        , appoggiatura :: Maybe Appoggiatura
        }
    | Rest
    | Clap
    deriving (Show, Eq)

data Pitch = Pitch
    { whiteKey :: WhiteKey
    , octaveTranspose :: Int
    , accidental :: Maybe Accidental
    }
    deriving (Show, Eq)

data WhiteKey = K1 | K2 | K3 | K4 | K5 | K6 | K7
    deriving (Show, Eq)

data Accidental = Natural | Sharp | Flat | DoubleSharp | DoubleFlat
    deriving (Show, Eq)

newtype Appoggiatura = Appoggiatura [Sound]
    deriving (Show, Eq)
