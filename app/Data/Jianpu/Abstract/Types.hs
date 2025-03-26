module Data.Jianpu.Abstract.Types where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Types
import Data.Ratio

newtype Music = Music [Voice] deriving (Show)

type Duration = Ratio Int

data Voice = Voice
    { voiceItems :: [VoiceItem]
    , tagSpans :: IM.IntervalMap Int TagSpan
    }
    deriving (Show)

data VoiceItem = VoiceItem
    { entity :: Entity
    , duration :: Duration
    }
    deriving (Show, Eq)

data Entity
    = Event Event
    | TagSingleton TagSingleton
    deriving (Show, Eq)

data TagSpan
    = Slur
    | Tie
    | Tuplet Int
    | Curve Boundary
    | Beam
    | Fermata
    | Lyrics [String]
    | TextBelow [String]
    deriving (Show, Eq)

{-
Closed    ┌─────┐
OpenLeft  ┌──────
OpenRight ──────┐
-}
data Boundary
    = Closed
    | OpenLeft
    | OpenRight
    deriving (Show, Eq)

data TagSingleton
    = BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    | BarLine
    | TimeSignature Int Int
    deriving (Show, Eq)

x = VoiceItem (Event (TimedEvent Whole 0 Clap)) 1
x' = VoiceItem (Event (TimedEvent Minim 0 Clap)) (1 % 2)
x'' = VoiceItem (Event (TimedEvent Crotchet 0 Clap)) (1 % 4)

entityLikeBarLine :: Entity -> Bool
entityLikeBarLine (TagSingleton tag) = tagLikeBarLine tag
entityLikeBarLine _ = False

tagLikeBarLine :: TagSingleton -> Bool
tagLikeBarLine BeginEndRepeat = True
tagLikeBarLine BeginRepeat = True
tagLikeBarLine EndRepeat = True
tagLikeBarLine DoubleBarLine = True
tagLikeBarLine EndSign = True
tagLikeBarLine BarLine = True
tagLikeBarLine _ = False
