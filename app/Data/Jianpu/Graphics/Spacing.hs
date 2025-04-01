module Data.Jianpu.Graphics.Spacing where

{-
I implement the spacing algorithm described in:

Kai Renz, geb. Flade.
Algorithms and Data Structures for a Music Notation System
based on GUIDO Music Notation.
Darmstadt, 2002.
https://guido.grame.fr/papers/kai_renz_diss.pdf
-}

import Control.Monad.State
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Graphics.Slice
import Data.List
import Data.List.NonEmpty (NonEmpty (..))

data SpringWithRod = SWR
    { rodLength :: Double
    , springConst :: Double
    }
    deriving (Show, Eq)

computeSmallestDurations :: MusicSlices -> [Duration]
computeSmallestDurations (MusicSlices []) = []
computeSmallestDurations (MusicSlices slices@((_, firstSlice) : _)) =
    evalState
        (traverse computeSmallestDurations' slices)
        (Nothing <$ firstSlice)
  where
    computeSmallestDurations' :: MusicSlice -> State [Maybe Duration] Duration
    computeSmallestDurations' (0, slice) =
        state $ const (0, Nothing <$ slice)
    computeSmallestDurations' (_, slice) = state $ \previousDurations ->
        let newPreviousDurations = zipWith update previousDurations slice
         in ( maybe 0 minimum (sequence newPreviousDurations)
            , newPreviousDurations
            )
      where
        update _ (Just (Right (Tag{}))) = Just 0
        update _ (Just (Right (Event{duration}))) = Just duration
        update _ (Just (Left (ContEvent _ (_, duration)))) = Just duration
        update previousDuration Nothing = previousDuration

findNeighbours :: [MusicSlice] -> [[MusicSlice]]
findNeighbours = groupBy $ \(_, slice1) (_, slice2) ->
    or (zipWith testNeighbour slice1 slice2)

testNeighbour :: Maybe (Either ContEvent Entity) -> Maybe (Either ContEvent Entity) -> Bool
testNeighbour
    (Just (Right (Event{duration = d1})))
    (Just (Right (Event{duration = d2}))) = d1 == d2
testNeighbour
    (Just (Right (Event{duration = d1})))
    (Just (Left (ContEvent _ (_, d2)))) = d1 == d2
testNeighbour
    (Just (Left (ContEvent _ (_, d1))))
    (Just (Right (Event{duration = d2}))) = d1 == d2
testNeighbour
    (Just (Left (ContEvent _ (_, d1))))
    (Just (Left (ContEvent _ (_, d2)))) = d1 == d2
testNeighbour _ _ = False

{- |
Given a string of springs with rods and its desired length,
returns the force needed to stretch them to that length.

SFF stands for Spring-Force-Function.
-}
sff :: NonEmpty SpringWithRod -> Double -> Double
sff springs@((springConst -> c1) :| _) x =
    if x <= xMinInit
        then 0
        else sff' springs xMinInit c1
  where
    xMinInit = sum (rodLength <$> springs)

    sff' (SWR{rodLength = xI} :| springs') xMin c =
        case springs' of
            [] -> f
            ((preStretchForce -> nextSpringPreStretchForce) : _)
                | f <= nextSpringPreStretchForce ->
                    f
            (spring'@(SWR{springConst = cI}) : springs'') ->
                sff' (spring' :| springs'') xMin' (1 / (1 / c + 1 / cI))
      where
        xMin' = xMin - xI
        f = (x - xMin') / c

        preStretchForce SWR{rodLength, springConst} = rodLength * springConst

gourlay'sSpacing ::
    Double -> Duration -> Double -> Duration -> Duration -> Double
gourlay'sSpacing a dMin dMinSpace ds di =
    recip (dMinSpace * (1 + a * logBase 2 (fromRational (toRational (di / dMin)))))
        * fromRational (toRational (di / ds))
