{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Split where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
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
so we need some clever ways to insert dummy elements (Nothing) so that all voices
have the same number of elements.
-}
sliceMusic ::
    Music ->
    Either SplitError [(Duration, NonEmpty (Either DummyItem VoiceItem))]
sliceMusic (Music voices) =
    case NE.nonEmpty voices of
        Nothing -> Left ErrorNoVoice
        Just voices' -> sliceMusic' (map Right . voiceItems <$> voices')

sliceMusic' ::
    NonEmpty [Either StagedItem VoiceItem] ->
    Either SplitError [(Duration, NonEmpty (Either DummyItem VoiceItem))]
sliceMusic' (NE.map uncons -> voicesHeadsTails) =
    case sequence voicesHeadsTails of
        -- All voices have got something
        Just headsTails -> do
            let (heads, tails) = NE.unzip headsTails
            let durations = NE.map getDuration heads
            if all (> 0) durations
                then sliceMusic'allHaveDuration durations heads tails
                else sliceMusic'someNoDuration durations heads tails
        -- Some voices are empty
        Nothing -> Right []

sliceMusic'allHaveDuration ::
    NonEmpty Duration ->
    NonEmpty (Either DummyItem VoiceItem) ->
    NonEmpty [Either StagedItem VoiceItem] ->
    Either SplitError [(Duration, NonEmpty (Either DummyItem VoiceItem))]
sliceMusic'allHaveDuration (NE.nub -> durations) heads tails =
    (slice :) <$> sliceMusic' voices'
  where
    minDuration = minimum durations
    (sliceResult, sliceTail) = NE.unzip $ processSlice minDuration <$> heads
    slice = (minDuration, sliceResult)
    voices' = NE.zipWith (++) (maybeToList <$> sliceTail) tails

sliceMusic'someNoDuration ::
    NonEmpty Duration ->
    NonEmpty (Either DummyItem VoiceItem) ->
    NonEmpty [Either StagedItem VoiceItem] ->
    Either SplitError [(Duration, NonEmpty (Either DummyItem VoiceItem))]
sliceMusic'someNoDuration (NE.nub -> durations) heads tails = undefined

debug voices = fmap (fmap (fmap getDuration)) <$> sliceMusic (Music ((`Voice` IM.empty) <$> voices))

getDuration :: Either StagedItem VoiceItem -> Duration
getDuration = \case
    Left duration -> duration
    Right VoiceItem{duration} -> duration

processSlice ::
    Duration ->
    Either StagedItem VoiceItem ->
    ( Either DummyItem VoiceItem
    , Maybe (Either StagedItem VoiceItem)
    )
processSlice minDuration (Left stagedItem) =
    ( Left minDuration
    , if stagedItem > minDuration
        then Just $ Left (stagedItem - minDuration)
        else Nothing
    )
processSlice minDuration (Right voiceItem@(VoiceItem{duration})) =
    ( Right voiceItem
    , if duration > minDuration
        then Just $ Left (duration - minDuration)
        else Nothing
    )
