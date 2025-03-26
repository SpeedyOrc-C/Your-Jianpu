module Data.Jianpu.Abstract.CalculateTiming where

import Control.Monad.State
import Data.Function
import Data.IntervalMap (IntervalMap)
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.Ratio

type TupletMultiplier = Ratio Int
type TimeSignature = (Int, Int)

calculateDurations :: IntervalMap Int Span -> [Either Tag Event] -> [Entity]
calculateDurations spans entities =
    evalState calculation Nothing
  where
    calculation = traverse (calculateDuration tuplets) (zip [0 ..] entities)

    tuplets = flip IM.mapMaybe spans $ \case
        Tuplet 2 -> Just $ 3 % 2
        Tuplet 3 -> Just $ 2 % 3
        Tuplet 5 -> Just $ 4 % 5
        _ -> Nothing

calculateDuration ::
    IntervalMap Int TupletMultiplier ->
    (Int, Either Tag Event) ->
    State (Maybe TimeSignature) Entity
calculateDuration _ (_, Left tagSingleton@(TimeSignature a b)) =
    state . const $ (Tag tagSingleton, Just (a, b))
calculateDuration _ (_, Left tagSingleton) =
    return . Tag $ tagSingleton
calculateDuration _ (_, Right event@(MultiBarRest bars)) =
    get >>= \case
        Nothing -> error "No time signature detected"
        Just (a, b) -> return $ Event event ((bars * a * 4) % b)
calculateDuration _ (_, Right event@Repeater4) =
    return $ Event event 1
calculateDuration tuplets (index, Right event@(Action{timeMultiplier, dot})) =
    return $ Event event (tuplet * baseDuration * dotExtension)
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
