{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Slice (MusicSlice, MusicSlices (..), sliceMusic, ContEvent(..)) where

import Data.Jianpu.Abstract.Types
import Data.Jianpu.Types
import Data.List
import Data.Maybe

newtype MusicSlices = MusicSlices [MusicSlice] deriving (Show)
type MusicSlice = (Duration, [SliceElement])
type SliceElement = Maybe (Either ContEvent Entity)

{-
Some notes start within other notes,
so this means a note that passes through a timestamp.
-}
data ContEvent
    = ContEvent
        Event
        ( Duration -- remaining duration
        , Duration -- total duration
        )
    deriving (Show)

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
sliceMusic (Music voices) =
    MusicSlices $ sliceMusic' (map Right . entities <$> voices)

sliceMusic' :: [[Either ContEvent Entity]] -> [MusicSlice]
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
                                then (Just (Right (Tag tag)), t)
                                else (Nothing, Right (Tag tag) : t)
                        _ -> error "This is not possible."
                Just (Left stagedElement@(ContEvent event (remainingDuration, totalDuration)), t) ->
                    ( Just (Left stagedElement)
                    , case alignedItem of
                        AlignTag _ -> Left stagedElement : t
                        AlignDuration alignedDuration ->
                            ( if alignedDuration < remainingDuration
                                then
                                    Left
                                        ( ContEvent
                                            event
                                            ( remainingDuration - alignedDuration
                                            , totalDuration
                                            )
                                        )
                                        : t
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
                            ( Just (Right (Event event duration))
                            , if alignDuration < duration
                                then
                                    Left
                                        ( ContEvent
                                            event
                                            ( duration - alignDuration
                                            , duration
                                            )
                                        )
                                        : t
                                else t
                            )

            alignedItem = minimum alignedItems

            alignedItems = flip map heads $ \case
                Left (ContEvent _ (stagedDuration, _)) ->
                    AlignDuration stagedDuration
                Right (Event _ duration) ->
                    AlignDuration duration
                Right (Tag tag) ->
                    AlignTag tag

            heads = map fst headsTails
