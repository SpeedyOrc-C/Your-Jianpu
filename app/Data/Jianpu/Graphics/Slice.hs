{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Slice (MusicSlice, MusicSlices, sliceMusic) where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.List
import Data.Maybe

data SplitError
    = ErrorNoVoice
    deriving (Show)

type MusicSlice = (Duration, [Maybe Entity])
type MusicSlices = [MusicSlice]

-- Used when we're in the middle of a longer event.
-- Its Duration is the remaining duration.
type StagedItem = Duration

data AlignedItem
    = AlignTag Tag
    | AlignDuration Duration
    deriving (Show, Eq, Ord)

{-
Slice a music by their timings. This works similar to matrix transposing,
but each voice may have different number of elements,
so we need some clever ways to insert dummy elements (Nothing) so that
all voices have the same number of elements.
-}
sliceMusic :: Music -> MusicSlices
sliceMusic (Music voices) = sliceMusic' (map Right . entities <$> voices)

sliceMusic' :: [[Either StagedItem Entity]] -> MusicSlices
sliceMusic' (map uncons -> maybeHeadTails) =
    case catMaybes maybeHeadTails of
        [] -> []
        headsTails -> result
          where
            result = slice : sliceMusic' newTails

            slice = (sliceDuration, newHeads)

            sliceDuration = case alignedItem of
                AlignTag _ -> 0
                AlignDuration d -> d

            (newHeads, newTails) = unzip newHeadsTails

            newHeadsTails = flip map maybeHeadTails $ \case
                Nothing -> (Nothing, [])
                Just (Right (Tag tag), t) ->
                    case alignedItem of
                        AlignTag alignedTag ->
                            if tag == alignedTag
                                then (Just (Tag tag), t)
                                else (Nothing, Right (Tag tag) : t)
                        _ -> error "This is not possible."
                Just (Left stagedItem, t) ->
                    ( Nothing
                    , case alignedItem of
                        AlignTag _ -> Left stagedItem : t
                        AlignDuration alignedDuration ->
                            ( if alignedDuration < stagedItem
                                then Left (stagedItem - alignedDuration) : t
                                else t
                            )
                    )
                Just (Right (Event event duration), t) ->
                    case alignedItem of
                        AlignTag _ ->
                            ( Nothing
                            , Right (Event event duration) : t
                            )
                        AlignDuration alignDuration ->
                            ( Just (Event event duration)
                            , if alignDuration < duration
                                then Left (duration - alignDuration) : t
                                else t
                            )

            alignedItem = minimum alignedItems

            alignedItems = flip map heads $ \case
                Left stagedDuration -> AlignDuration stagedDuration
                Right (Event _ duration) -> AlignDuration duration
                Right (Tag tag) -> AlignTag tag

            heads = map fst headsTails

debug voices = sliceMusic (Music ((`Voice` IM.empty) <$> voices))
