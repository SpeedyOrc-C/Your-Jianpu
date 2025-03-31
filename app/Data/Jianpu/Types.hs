module Data.Jianpu.Types where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe

data Event
    = Repeater4
    | MultiBarRest Int
    | Action
        { timeMultiplier :: TimeMultiplier
        , dot :: Int
        , sound :: Sound
        }
    | Pronounce (Maybe Syllable)
    deriving (Eq)

data TimeMultiplier = Whole | Minim | Crotchet | Quaver | Semiquaver
    deriving (Eq)

data Sound
    = Note
        { pitches :: NonEmpty Pitch
        , appoggiatura :: Maybe Appoggiatura
        }
    | Rest
    | Clap
    deriving (Eq)

data Pitch = Pitch
    { whiteKey :: WhiteKey
    , octaveTranspose :: Int
    , accidental :: Maybe Accidental
    }
    deriving (Eq)

data Syllable = Syllable
    { prefix :: Maybe String
    , content :: String
    , suffix :: Maybe String
    -- Prefixes and suffixes are usually punctuations
    }
    deriving (Show, Eq)

data WhiteKey = K1 | K2 | K3 | K4 | K5 | K6 | K7
    deriving (Eq)

data Accidental = Natural | Sharp | Flat | DoubleSharp | DoubleFlat
    deriving (Eq)

newtype Appoggiatura = Appoggiatura [Sound]
    deriving (Show, Eq)

instance Show Event where
    show :: Event -> String
    show Repeater4 = "-"
    show (MultiBarRest n) = "==" ++ show n ++ "=="
    show Action{..} = show sound ++ show timeMultiplier ++ replicate dot '*'
    show (Pronounce Nothing) = "_"
    show (Pronounce (Just Syllable{..})) =
        fromMaybe "" prefix ++ content ++ fromMaybe "" suffix

instance Show TimeMultiplier where
    show :: TimeMultiplier -> String
    show Whole = ""
    show Minim = "/"
    show Crotchet = "//"
    show Quaver = "///"
    show Semiquaver = "////"

instance Show Sound where
    show :: Sound -> String
    show Rest = "0"
    show Clap = "X"
    show Note{..} =
        case pitches of
            p :| [] -> show p
            p :| ps -> "[" ++ unwords (show <$> p : ps) ++ "]"

instance Show Pitch where
    show :: Pitch -> String
    show Pitch{..} =
        maybe "" show accidental
            ++ show whiteKey
            ++ ( if octaveTranspose > 0
                    then replicate octaveTranspose '\''
                    else if octaveTranspose < 0
                        then replicate (negate octaveTranspose) '.'
                        else ""
               )

instance Show Accidental where
    show :: Accidental -> String
    show Natural = "="
    show Sharp = "#"
    show Flat = "b"
    show DoubleSharp = "x"
    show DoubleFlat = "bb"

instance Show WhiteKey where
    show :: WhiteKey -> String
    show K1 = "1"
    show K2 = "2"
    show K3 = "3"
    show K4 = "4"
    show K5 = "5"
    show K6 = "6"
    show K7 = "7"
