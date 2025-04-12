module Data.Jianpu.Graphics where
import Data.Jianpu.Types ( Event, Duration )
import Data.IntervalMap (IntervalMap)
import Data.Jianpu.Abstract ( Span, Entity )

data Slices = Slices [Slice] [IntervalMap Int Span]
    deriving (Show)

data Slice = Slice
    { duration :: Duration
    , elements :: [SliceElement]
    }
    deriving (Show)

type SliceElement = Maybe (Either ContEvent Entity)

{-
Some notes start within other notes,
so this means a note that passes through a timestamp.
-}
data ContEvent
    = ContEvent
        Event
        ( Duration -- remaining duration
        , Duration -- total duration
        )
    deriving (Show)
