{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Split where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Utils
import Data.Maybe (maybeToList)
import Debug.Trace (traceShowM)

data SplitError
    = ErrorNoVoice
    deriving (Show)

-- Dummy entity to fill in the gaps
type DummyItem = Duration

-- Used when we're in the middle of a longer event.
-- Its Duration is the remaining duration.
type StagedItem = Duration

{-
Slice a music by their timings. This works similar to matrix transposing,
but each voice may have different number of elements,
so we need some clever ways to insert dummy elements (Nothing) so that
all voices have the same number of elements.
-}
sliceMusic ::
    Music ->
    Either SplitError [(Duration, NonEmpty (Either DummyItem Entity))]
sliceMusic (Music voices) =
    case NE.nonEmpty voices of
        Nothing -> Left ErrorNoVoice
        Just voices' -> sliceMusic' (map Right . entities <$> voices')
  where
    sliceMusic' ::
        NonEmpty [Either StagedItem Entity] ->
        Either SplitError [(Duration, NonEmpty (Either DummyItem Entity))]
    sliceMusic' (NE.map uncons -> voicesHeadsTails) =
        case sequence voicesHeadsTails of
            -- All voices have got something
            Just headsTails -> do
                let (heads, tails) = NE.unzip headsTails
                case partitionEithersNE (pullTagOut <$> heads) of
                    Right eventsAndStagedItems ->
                        sliceMusic'allHaveDuration eventsAndStagedItems tails
                    Left tags ->
                        sliceMusic'someTags tags heads tails
            -- Some voices are empty
            Nothing -> Right []
      where
        pullTagOut ::
            Either StagedItem Entity ->
            Either Tag (Either StagedItem (Event, Duration))
        pullTagOut (Left s) = Right (Left s)
        pullTagOut (Right (Event e d)) = Right (Right (e, d))
        pullTagOut (Right (Tag t)) = Left t

        sliceMusic'allHaveDuration ::
            NonEmpty (Either DummyItem (Event, Duration)) ->
            NonEmpty [Either StagedItem Entity] ->
            Either SplitError [(Duration, NonEmpty (Either DummyItem Entity))]
        sliceMusic'allHaveDuration heads tails =
            (slice :) <$> sliceMusic' voices'
          where
            minDuration = minimum $ getDuration <$> heads
            (sliceResult, sliceTail) = NE.unzip $ getSlice <$> heads
            slice = (minDuration, sliceResult)
            voices' = NE.zipWith (++) (maybeToList <$> sliceTail) tails

            getSlice ::
                Either StagedItem (Event, Duration) ->
                (Either DummyItem Entity, Maybe (Either StagedItem Entity))
            getSlice (Left stagedItem) =
                ( Left minDuration
                , if stagedItem > minDuration
                    then Just $ Left (stagedItem - minDuration)
                    else Nothing
                )
            getSlice (Right (event, duration)) =
                ( Right (Event event minDuration)
                , if duration > minDuration
                    then Just $ Left (duration - minDuration)
                    else Nothing
                )

        sliceMusic'someTags ::
            NonEmpty Tag ->
            NonEmpty (Either DummyItem Entity) ->
            NonEmpty [Either StagedItem Entity] ->
            Either SplitError [(Duration, NonEmpty (Either DummyItem Entity))]
        sliceMusic'someTags tags heads tails =
            (slice :) <$> sliceMusic' voices'
          where
            focusedTag = minimum tags
            (sliceResult, sliceTail) = NE.unzip $ getSlice focusedTag <$> heads
            slice = (0, sliceResult)
            voices' = NE.zipWith (++) (maybeToList <$> sliceTail) tails

            getSlice ::
                Tag ->
                Either StagedItem Entity ->
                (Either DummyItem Entity, Maybe (Either StagedItem Entity))
            getSlice _ entity@(Left{}) = (Left 0, Just entity)
            getSlice _ entity@(Right Event{}) = (Left 0, Just entity)
            getSlice tag entity@(Right (Tag tag')) =
                if tag == tag'
                    then (entity, Nothing)
                    else (Left 0, Just entity)

        getDuration :: Either StagedItem (Event, Duration) -> Duration
        getDuration = \case
            Left stagedDuration -> stagedDuration
            Right (_, duration) -> duration

debug voices = sliceMusic (Music ((`Voice` IM.empty) <$> voices))
