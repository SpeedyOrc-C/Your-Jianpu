module Data.Jianpu.Abstract.CalculateTiming where

import Control.Monad.State
import Data.Function
import Data.IntervalMap (IntervalMap)
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.Ratio
import Data.Maybe

type TupletMultiplier = Ratio Int
type TimeSignature = (Int, Int)

calculateDurations :: IntervalMap Int TagSpan -> [Entity] -> [Duration]
calculateDurations spans entities =
    map (fromMaybe 0) durations
  where
    durations = evalState calculation Nothing

    calculation = traverse (calculateDuration tuplets) (zip [0 ..] entities)

    tuplets = flip IM.mapMaybe spans $ \case
        Tuplet 2 -> Just $ 3 % 2
        Tuplet 3 -> Just $ 2 % 3
        Tuplet 5 -> Just $ 4 % 5
        _ -> Nothing

calculateDuration ::
    IntervalMap Int TupletMultiplier ->
    (Int, Entity) ->
    State (Maybe TimeSignature) (Maybe Duration)
calculateDuration _ (_, TagSingleton (TimeSignature a b)) =
    state . const $ (Nothing, Just (a, b))
calculateDuration _ (_, TagSingleton _) =
    return Nothing
calculateDuration _ (_, Event (MultiBarRest bars)) =
    get >>= \case
        Nothing -> error "No time signature detected"
        Just (a, b) -> return . Just $ (bars * a * 4) % b
calculateDuration _ (_, Event Repeater4) =
    return $ Just 1
calculateDuration tuplets (index, Event (TimedEvent{timeMultiplier, dot})) =
    return . Just $ tuplet * baseDuration * dotExtension
  where
    tuplet =
        tuplets `IM.containing` index
            & IM.toList
            & map snd
            & product

    baseDuration = case timeMultiplier of
        Whole -> 1
        Minim -> 1 % 2
        Crotchet -> 1 % 4
        Quaver -> 1 % 8
        Semiquaver -> 1 % 16

    dotExtension = case dot of
        0 -> 1
        n -> 2 - (1 % (2 ^ n))
