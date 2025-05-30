module Data.Jianpu.Document.Beaming (addBeams) where

import Control.Monad.Reader (MonadTrans (lift), asks)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Jianpu.Abstract (Entity (Event, Tag, duration, event), Span (Beam), Tag (TimeSignature), Interval (I))
import Data.Jianpu.Abstract.Error (HasError)
import Data.Jianpu.Document (DocumentVoice (..))
import Data.Jianpu.Graphics.Config (RenderConfig (initialTimeSignature), RenderContextT)
import Data.Jianpu.Types (Duration, Event (Action), TimeSignature)
import Data.Ratio ((%))

data BeamingState = BS
    { nowDuration :: Duration
    , nowMaxDuration :: Duration
    , spanLeftIndex :: Int
    }
    deriving (Show)

addBeams :: DocumentVoice -> RenderContextT HasError DocumentVoice
addBeams voice@DVoice{..} = do
    initialTimeSignature <- asks initialTimeSignature

    beamSpans <- lift $ do
        initialGroupDuration <- groupDurationOf initialTimeSignature

        evalStateT
            (addBeams' (zip [0 ..] dEntities))
            BS
                { nowDuration = 0
                , spanLeftIndex = 0
                , nowMaxDuration = initialGroupDuration
                }

    pure $ voice{dSpans = dSpans <> IM.fromList beamSpans}

addBeams' :: [(Int, Entity)] -> StateT BeamingState HasError [(Interval Int, Span)]
addBeams' [] = pure []
addBeams' ((index, Tag (TimeSignature a b)) : es) = do
    groupDuration <- lift $ groupDurationOf (a, b)

    put
        BS
            { nowDuration = 0
            , spanLeftIndex = index + 1
            , nowMaxDuration = groupDuration
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
            ((I (spanLeftIndex, index), Beam) :) <$> addBeams' es
        LT -> do
            put BS{nowDuration = nowDuration + duration, ..}
            addBeams' es
        GT -> error "Incorrect notes duration."
addBeams' ((index, _) : es) = do
    BS{..} <- get
    put BS{nowDuration = 0, spanLeftIndex = index + 1, ..}
    addBeams' es

groupDurationOf :: TimeSignature -> HasError Duration
groupDurationOf (2, 4) = pure 1
groupDurationOf (3, 4) = pure 1
groupDurationOf (4, 4) = pure 2
groupDurationOf (3, 8) = pure (3 % 2)
groupDurationOf (6, 8) = pure (3 % 2)
groupDurationOf (9, 8) = pure (3 % 2)
groupDurationOf (12, 8) = pure (3 % 2)
groupDurationOf _ = Left undefined
