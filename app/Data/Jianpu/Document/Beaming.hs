module Data.Jianpu.Document.Beaming (addBeams) where

import Control.Monad.Reader (asks)
import Control.Monad.State (MonadState (..), State, evalState)
import Data.IntervalMap (Interval (ClosedInterval))
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract (Entity (Event, Tag, duration, event), Span (Beam), Tag (TimeSignature))
import Data.Jianpu.Document (DocumentVoice (..))
import Data.Jianpu.Graphics.Config (RenderConfig (initialTimeSignature), RenderContext)
import Data.Jianpu.Types (Duration, Event (Action), TimeSignature)
import Data.Ratio ((%))

data BeamingState = BS
    { nowDuration :: Duration
    , nowMaxDuration :: Duration
    , spanLeftIndex :: Int
    }
    deriving (Show)

addBeams :: DocumentVoice -> RenderContext DocumentVoice
addBeams voice@DVoice{..} = do
    initialTimeSignature <- asks initialTimeSignature

    let beamSpans =
            evalState (addBeams' (zip [0 ..] dEntities)) $
                BS
                    { nowDuration = 0
                    , spanLeftIndex = 0
                    , nowMaxDuration = groupDurationOf initialTimeSignature
                    }

    pure voice{dSpans = dSpans <> IM.fromList beamSpans}

addBeams' :: [(Int, Entity)] -> State BeamingState [(Interval Int, Span)]
addBeams' [] = pure []
addBeams' ((index, Tag (TimeSignature a b)) : es) = do
    put
        BS
            { nowDuration = 0
            , spanLeftIndex = index + 1
            , nowMaxDuration = groupDurationOf (a, b)
            }
    addBeams' es
addBeams' ((index, Event{duration, event = Action{}}) : es) = do
    BS{..} <- get
    let newDuration = nowDuration + duration
    case compare newDuration nowMaxDuration of
        EQ -> do
            put
                BS
                    { nowDuration = 0
                    , spanLeftIndex = index + 1
                    , ..
                    }
            ((ClosedInterval spanLeftIndex index, Beam) :) <$> addBeams' es
        LT -> do
            put BS{nowDuration = nowDuration + duration, ..}
            addBeams' es
        GT -> error "Incorrect notes duration."
addBeams' ((index, _) : es) = do
    BS{..} <- get
    put BS{nowDuration = 0, spanLeftIndex = index + 1, ..}
    addBeams' es

groupDurationOf :: TimeSignature -> Duration
groupDurationOf (2, 4) = 1
groupDurationOf (3, 4) = 1
groupDurationOf (4, 4) = 2
groupDurationOf (3, 8) = 3 % 2
groupDurationOf (6, 8) = 3 % 2
groupDurationOf (9, 8) = 3 % 2
groupDurationOf (12, 8) = 3 % 2
groupDurationOf _ = error "Unsupported time signature"
