{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Data.Jianpu.Graphics.Slice where

import Control.Arrow
import Control.Monad.State
import Data.IntervalMap (IntervalMap)
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types qualified as Abstract
import Data.Jianpu.Types
import Data.List
import Data.Maybe

data Slices = Slices [Slice] [IntervalMap Int Abstract.Span]
    deriving (Show)

data Slice = Slice
    { duration :: Duration
    , elements :: [SliceElement]
    }
    deriving (Show)

type SliceElement = Maybe (Either ContEvent Abstract.Entity)

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
    = AlignTag Abstract.Tag
    | AlignDuration Duration
    deriving (Show, Eq, Ord)

{-
Slice a music by their timings. This works similar to matrix transposing,
but each voice may have different number of elements,
so we need some clever ways to insert dummy elements (Nothing) so that
all voices have the same number of elements.

The indices of spans will shift as well because of the new dummy elements.
-}
sliceMusic :: Abstract.Music -> Slices
sliceMusic (Abstract.Music voices) =
    Slices (zipWith Slice durations elementsGroups) spansGroups
  where
    (durations, elementsGroups) =
        unzip $ sliceMusic' (map Right . Abstract.entities <$> voices)
    spansGroups = zipWith ($) spansMappers (Abstract.spans <$> voices)
    spansMappers = computeSpansMapper <$> slicesByVoice
    slicesByVoice = transpose elementsGroups

computeSpansMapper ::
    [SliceElement] ->
    (IntervalMap Int Abstract.Span -> IntervalMap Int Abstract.Span)
computeSpansMapper = do
    fmap (\case Just (Right{}) -> True; _ -> False)
        >>> computeOldToNewIndices
        >>> (\pairs a -> fromJust (lookup a pairs))
        >>> (\f -> IM.toList >>> fmap (first (fmap f)) >>> IM.fromList)

computeOldToNewIndices :: [Bool] -> [(Int, Int)]
computeOldToNewIndices mask = evalState (f mask) (0, 0)
  where
    f [] = pure []
    f (False : mask') = do
        (old, new) <- get
        put (old, new + 1)
        f mask'
    f (True : mask') = do
        (old, new) <- get
        put (old + 1, new + 1)
        ((old, new) :) <$> f mask'

sliceMusic' ::
    [[Either ContEvent Abstract.Entity]] -> [(Duration, [SliceElement])]
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
                Just (Right (Abstract.Tag tag), t) ->
                    case alignedItem of
                        AlignTag alignedTag ->
                            if tag == alignedTag
                                then (Just (Right (Abstract.Tag tag)), t)
                                else (Nothing, Right (Abstract.Tag tag) : t)
                        _ -> error "This is not possible."
                Just (Left cont@(ContEvent event (remainingDuration, totalDuration)), t) ->
                    ( Just (Left cont)
                    , case alignedItem of
                        AlignTag _ -> Left cont : t
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
                Just (Right (Abstract.Event event duration), t) ->
                    case alignedItem of
                        AlignTag _ ->
                            ( Nothing
                            , Right (Abstract.Event event duration) : t
                            )
                        AlignDuration alignDuration ->
                            ( Just (Right (Abstract.Event event duration))
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
                Right (Abstract.Event _ duration) ->
                    AlignDuration duration
                Right (Abstract.Tag tag) ->
                    AlignTag tag

            heads = map fst headsTails
