module Data.Jianpu.Abstract where

import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Jianpu.Types
import Data.Ratio

newtype Music = Music [Voice] deriving (Show)

type Spans = IM.IntervalMap (Interval Int) Span

data Voice = Voice
    { entities :: [Entity]
    , spans :: Spans
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
    | Beam
    | Fermata
    deriving (Show, Eq, Ord)

data Boundary
    = Closed
    | LeftOpened
    | RightOpened
    deriving (Show, Eq)

newtype Interval a = I (a, a)
    deriving (Show, Eq, Ord)

newtype SpanWithRenderOrder = SRO (Interval Int, Span)
    deriving (Show)

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

isCurve :: Span -> Bool
isCurve Slur = True
isCurve Tuplet{} = True
isCurve Tie = True
isCurve _ = False

instance IM.Interval (Interval Int) Int where
    lowerBound :: Interval Int -> Int
    lowerBound (I (a, _)) = a

    upperBound :: Interval Int -> Int
    upperBound (I (_, b)) = b

instance IM.Interval (Interval Double) Double where
    lowerBound :: Interval Double -> Double
    lowerBound (I (a, _)) = a

    upperBound :: Interval Double -> Double
    upperBound (I (_, b)) = b

-- equal: Render order of 2 spans doesn't matter
-- not equal: It matters!
instance Eq SpanWithRenderOrder where
    (==) :: SpanWithRenderOrder -> SpanWithRenderOrder -> Bool
    SRO (I (a1, b1), s1) == SRO (I (a2, b2), s2)
        | s1 == s2 && a1 == a2 && b1 == b2 = True
        | otherwise = False

instance Ord SpanWithRenderOrder where
    compare :: SpanWithRenderOrder -> SpanWithRenderOrder -> Ordering
    compare (SRO (I (a1, b1), s1)) (SRO (I (a2, b2), s2))
        | s1 == s2 && a1 == a2 && b1 == b2 = EQ
        | b1 < a2 = LT
        | b2 < a1 = GT
        | isCurve s1 && isCurve s2 && a2 <= a1 && b1 <= b2 = LT
        | isCurve s1 && isCurve s2 && a1 <= a2 && b2 <= b1 = GT
        | isCurve s1 && isCurve s2 && b1 == a2 = LT
    compare (SRO (I (a1, b1), s1)) (SRO (I (a2, b2), Fermata))
        | isCurve s1 = GT
    compare s1 s2@(SRO (_, Tuplet{})) = compare s2 s1
    compare (SRO (_, Beam)) s2 = GT
    compare s1 s2@(SRO (_, Beam)) = compare s2 s1
    compare a b = error (show (a, b))

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
