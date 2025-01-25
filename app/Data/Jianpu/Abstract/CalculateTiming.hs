module Data.Jianpu.Abstract.CalculateTiming where

import Control.Monad.State
import Data.Function
import Data.IntervalMap (IntervalMap)
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.Ratio

type TupletMultiplier = Ratio Int
type Duration = Ratio Int
type TimeSignature = (Int, Int)

calculateTiming :: IntervalMap Int TagSpan -> [Entity] -> [Timing]
calculateTiming spans entities =
    accumulateDurations 0 durations
  where
    durations = evalState calculation Nothing

    calculation = traverse (calculateOne tuplets) (zip [0 ..] entities)

    tuplets = flip IM.mapMaybe spans $ \case
        Tuplet 2 -> Just $ 3 % 2
        Tuplet 3 -> Just $ 2 % 3
        Tuplet 5 -> Just $ 4 % 5
        _ -> Nothing

accumulateDurations :: Duration -> [Maybe Duration] -> [Timing]
accumulateDurations _ [] = []
accumulateDurations t (Nothing : ds) = Timing t 0 : accumulateDurations t ds
accumulateDurations t (Just t0 : ds) = Timing t t0 : accumulateDurations (t + t0) ds

calculateOne ::
    IntervalMap Int TupletMultiplier ->
    (Int, Entity) ->
    State (Maybe TimeSignature) (Maybe Duration)
calculateOne _ (_, TagSingleton (TimeSignature a b)) =
    state . const $ (Nothing, Just (a, b))
calculateOne _ (_, TagSingleton _) =
    return Nothing
calculateOne _ (_, Event (MultiBarRest bars)) =
    get >>= \case
        Nothing -> error "No time signature detected"
        Just (a, b) -> return . Just $ (bars * a * 4) % b
calculateOne _ (_, Event Repeater4) =
    return $ Just 1
calculateOne tuplets (index, Event (TimedEvent{timeMultiplier, dot})) =
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
