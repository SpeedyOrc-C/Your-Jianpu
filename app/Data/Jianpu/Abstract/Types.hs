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
Closed       ┌─────┐
LeftOpened   ┌──────
RightOpened  ──────┐
-}
data Boundary
    = Closed
    | LeftOpened
    | RightOpened
    deriving (Show, Eq)

data Tag
    = TimeSignature Int Int
    | BarLine
    | BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    deriving (Show, Eq, Ord)

x = Event (Action Whole 0 Clap) 1
x' = Event (Action Minim 0 Clap) (1 % 2)
x'' = Event (Action Crotchet 0 Clap) (1 % 4)
b = Tag BarLine

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
