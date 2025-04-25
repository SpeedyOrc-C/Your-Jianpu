module Data.Jianpu.Syntax.CalculateDurations (calculateDurations) where

import Control.Monad.State (
    MonadState (get, state),
    State,
    evalState,
 )
import Data.Function ((&))
import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Jianpu.Abstract (
    Entity (Event, Tag),
    Interval,
    Span (Tuplet),
    Spans,
    Tag (TimeSignature),
 )
import Data.Jianpu.Types (
    Event (
        Action,
        MultiBarRest,
        Pronounce,
        Repeater4,
        dot,
        timeMultiplier
    ),
    TimeMultiplier (Crotchet, Minim, Quaver, Semiquaver, Whole),
    TimeSignature,
 )
import Data.Ratio (Ratio, (%))

type TupletMultiplier = Ratio Int

calculateDurations :: (Spans, [Either Tag Event]) -> [Entity]
calculateDurations (spans, entities) =
    evalState calculation Nothing
  where
    calculation = traverse (calculateDuration tuplets) (zip [0 ..] entities)

    tuplets = flip IM.mapMaybe spans $ \case
        Tuplet 2 -> Just $ 3 % 2
        Tuplet 3 -> Just $ 2 % 3
        Tuplet 5 -> Just $ 4 % 5
        _ -> Nothing

calculateDuration ::
    IM.IntervalMap (Interval Int) TupletMultiplier ->
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
calculateDuration _ (_, Right (Pronounce{})) =
    error "Pronunciation's duration depends on the main voice above it, this is calculated in module GenerateLyricsVoices."
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
