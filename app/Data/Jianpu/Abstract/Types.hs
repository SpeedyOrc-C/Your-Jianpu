module Data.Jianpu.Abstract.Types where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Types
import Data.Ratio

newtype Music = Music [Voice] deriving (Show)

data Voice = Voice
    { voiceItems :: [VoiceItem]
    , tagSpans :: IM.IntervalMap Int TagSpan
    }
    deriving (Show)

data VoiceItem = VoiceItem {
    entity :: Entity,
    timing :: Timing
} deriving Show

data Timing = Timing
    { startBeat :: Ratio Int
    , duration :: Ratio Int
    }
    deriving (Show)

data Entity
    = Event Event
    | TagSingleton TagSingleton
    deriving (Show)

data TagSpan
    = Slur
    | Tie
    | Tuplet Int
    | Curve Boundary
    | Beam
    | Fermata
    | Lyrics [String]
    | TextBelow [String]
    deriving (Show)

{-
Closed    ┌─────┐
OpenLeft  ┌──────
OpenRight ──────┐
-}
data Boundary
    = Closed
    | OpenLeft
    | OpenRight
    deriving Show

data TagSingleton
    = BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    | BarLine
    | TimeSignature Int Int
    deriving (Show)

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
