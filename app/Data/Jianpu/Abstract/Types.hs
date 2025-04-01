module Data.Jianpu.Abstract.Types where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Types
import Data.Ratio

newtype Music = Music [Voice] deriving (Show)

type Duration = Ratio Int

data Voice
    = Voice
    { entities :: [Entity]
    , tagSpans :: IM.IntervalMap Int Span
    }
    deriving (Show)

data Entity
    = Event {event :: Event, duration :: Duration}
    | Tag Tag
    deriving (Eq)

data Tag
    = TimeSignature Int Int
    | BarLine
    | BeginEndRepeat
    | BeginRepeat
    | EndRepeat
    | DoubleBarLine
    | EndSign
    deriving (Eq, Ord)

data Span
    = Slur
    | Tie -- Connects two notes of the same pitch
    | TieInChord [Int] -- Between two chords, connects the notes at the specified indices
    | Tuplet Int
    | Curve Boundary
    | Beam
    | Fermata
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

instance Show Entity where
    show :: Entity -> String
    show Event{..} =
        show event
            ++ " "
            ++ (show (numerator duration) ++ "%" ++ show (denominator duration))
    show (Tag tag) = show tag

instance Show Tag where
    show :: Tag -> String
    show (TimeSignature a b) = show a ++ "/" ++ show b
    show BarLine = "|"
    show BeginEndRepeat = ":|:"
    show BeginRepeat = "|:"
    show EndRepeat = ":|"
    show DoubleBarLine = "||"
    show EndSign = "|||"

-- x = Event (Action Whole 0 Clap) 1
-- x' = Event (Action Minim 0 Clap) (1 % 2)
-- x'' = Event (Action Crotchet 0 Clap) (1 % 4)
-- b = Tag BarLine

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

doubleFromDuration :: Duration -> Double
doubleFromDuration d = fromIntegral (numerator d) / fromIntegral (denominator d)
