module Data.Jianpu.Abstract.Types where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Types
import Data.Ratio

newtype Music = Music [Voice] deriving (Show)

type Duration = Ratio Int

data Voice = Voice
    { entities :: [Entity]
    , tagSpans :: IM.IntervalMap Int Span
    }
    deriving (Show)

data Entity
    = Event {event :: Event, duration :: Duration}
    | Tag Tag
    deriving (Show, Eq)

data Span
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

data Tag
    = BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    | BarLine
    | TimeSignature Int Int
    deriving (Show, Eq)

-- x = VoiceItem (Event (TimedEvent Whole 0 Clap)) 1
-- x' = VoiceItem (Event (TimedEvent Minim 0 Clap)) (1 % 2)
-- x'' = VoiceItem (Event (TimedEvent Crotchet 0 Clap)) (1 % 4)

entityLikeBarLine :: Entity -> Bool
entityLikeBarLine (Tag tag) = tagLikeBarLine tag
entityLikeBarLine _ = False

tagLikeBarLine :: Tag -> Bool
tagLikeBarLine BeginEndRepeat = True
tagLikeBarLine BeginRepeat = True
tagLikeBarLine EndRepeat = True
tagLikeBarLine DoubleBarLine = True
tagLikeBarLine EndSign = True
tagLikeBarLine BarLine = True
tagLikeBarLine _ = False
