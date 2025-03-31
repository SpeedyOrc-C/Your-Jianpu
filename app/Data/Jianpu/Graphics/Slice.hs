{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Slice (MusicSlice, MusicSlices, sliceMusic) where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Utils
import Data.Maybe (maybeToList)

data SplitError
    = ErrorNoVoice
    deriving (Show)

type MusicSlice = (Duration, NonEmpty (Maybe Entity))
type MusicSlices = [MusicSlice]

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
    Either SplitError MusicSlices
sliceMusic (Music voices) =
    case NE.nonEmpty voices of
        Nothing -> Left ErrorNoVoice
        Just voices' -> sliceMusic' (map Right . entities <$> voices')
  where
    sliceMusic' ::
        NonEmpty [Either StagedItem Entity] ->
        Either SplitError MusicSlices
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
            -- TODO: usually a complete music should have all voices filled
            --       so I'll leave this for now
            Nothing -> Right []
      where
        pullTagOut ::
            Either StagedItem Entity ->
            Either Tag (Either StagedItem (Event, Duration))
        pullTagOut (Left s) = Right (Left s)
        pullTagOut (Right (Event e d)) = Right (Right (e, d))
        pullTagOut (Right (Tag t)) = Left t

        sliceMusic'allHaveDuration ::
            NonEmpty (Either StagedItem (Event, Duration)) ->
            NonEmpty [Either StagedItem Entity] ->
            Either SplitError MusicSlices
        sliceMusic'allHaveDuration heads tails =
            (slice :) <$> sliceMusic' voices'
          where
            minDuration = minimum $ getDuration <$> heads
            (sliceResult, sliceTail) = NE.unzip $ getSlice <$> heads
            slice = (minDuration, sliceResult)
            voices' = NE.zipWith (++) (maybeToList <$> sliceTail) tails

            getSlice ::
                Either StagedItem (Event, Duration) ->
                (Maybe Entity, Maybe (Either StagedItem Entity))
            getSlice (Left stagedItem) =
                ( Nothing
                , if stagedItem > minDuration
                    then Just $ Left (stagedItem - minDuration)
                    else Nothing
                )
            getSlice (Right (event, duration)) =
                ( Just (Event event minDuration)
                , if duration > minDuration
                    then Just $ Left (duration - minDuration)
                    else Nothing
                )

        sliceMusic'someTags ::
            NonEmpty Tag ->
            NonEmpty (Either StagedItem Entity) ->
            NonEmpty [Either StagedItem Entity] ->
            Either SplitError MusicSlices
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
                (Maybe Entity, Maybe (Either StagedItem Entity))
            getSlice _ stagedItem@(Left{}) = (Nothing, Just stagedItem)
            getSlice _ entity@(Right Event{}) = (Nothing, Just entity)
            getSlice tag entity@(Right (Tag tag')) =
                if tag == tag'
                    then (Just (Tag tag'), Nothing)
                    else (Nothing, Just entity)

        getDuration :: Either StagedItem (Event, Duration) -> Duration
        getDuration = \case
            Left stagedDuration -> stagedDuration
            Right (_, duration) -> duration

debug voices = sliceMusic (Music ((`Voice` IM.empty) <$> voices))
