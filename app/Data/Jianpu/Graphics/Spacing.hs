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
import Data.Jianpu.Abstract qualified as Abstract
import Data.Jianpu.Graphics
import Data.Jianpu.Types
import Data.List
import Data.List.NonEmpty (NonEmpty (..))

-- TODO: Using Gourlay's for now, implement improved algorithm later...
computeSpringConstants :: [Slice] -> [Double]
computeSpringConstants slices =
    configuredGourlayF <$> embedSmallestDurations
  where
    configuredGourlayF = gourlayF 0.5 minDuration 1

    minDuration = minimum . filter (> 0) . map duration $ slices

    embedSmallestDurations =
        zipWith
            ( \smallestDuration (Slice{duration = springDuration}) ->
                AuxD{smallestDuration, springDuration}
            )
            smallestDurations
            slices

    smallestDurations = computeSmallestDurations slices

computeSmallestDurations :: [Slice] -> [Duration]
computeSmallestDurations [] = []
computeSmallestDurations slices@(Slice{elements = firstSlice} : _) =
    evalState
        (traverse computeSmallestDurations' slices)
        (Nothing <$ firstSlice)
  where
    -- TODO: simplify this
    computeSmallestDurations' :: Slice -> State [Maybe Duration] Duration
    computeSmallestDurations' Slice{duration = 0, elements = slice} =
        state $ const (0, Nothing <$ slice)
    computeSmallestDurations' Slice{elements = slice} = state $ \previousDurations ->
        let newPreviousDurations = zipWith update previousDurations slice
         in ( maybe 0 minimum (sequence newPreviousDurations)
            , newPreviousDurations
            )
      where
        update _ (Just (Right (Abstract.Tag{}))) = Just 0
        update _ (Just (Right (Abstract.Event{duration}))) = Just duration
        update _ (Just (Left (ContEvent _ (_, duration)))) = Just duration
        update previousDuration Nothing = previousDuration

groupByNeighbours :: [Slice] -> [[Slice]]
groupByNeighbours = groupBy $ \(Slice{elements = slice1}) (Slice{elements = slice2}) ->
    or (zipWith testNeighbour slice1 slice2)

testNeighbour ::
    Maybe (Either ContEvent Abstract.Entity) ->
    Maybe (Either ContEvent Abstract.Entity) ->
    Bool
testNeighbour
    (Just (Right (Abstract.Event{duration = d1})))
    (Just (Right (Abstract.Event{duration = d2}))) = d1 == d2
testNeighbour
    (Just (Right (Abstract.Event{duration = d1})))
    (Just (Left (ContEvent _ (_, d2)))) = d1 == d2
testNeighbour
    (Just (Left (ContEvent _ (_, d1))))
    (Just (Right (Abstract.Event{duration = d2}))) = d1 == d2
testNeighbour
    (Just (Left (ContEvent _ (_, d1))))
    (Just (Left (ContEvent _ (_, d2)))) = d1 == d2
testNeighbour _ _ = False

data AuxDurations = AuxD
    { smallestDuration :: Duration
    , springDuration :: Duration
    }

{- |
Gourlay's spacing function
-}
gourlayF :: Double -> Duration -> Double -> AuxDurations -> Double
gourlayF _ _ _ (AuxD{springDuration = 0}) = 1 / 0
gourlayF
    magic
    minDuration
    minDurationWidth
    (AuxD{smallestDuration, springDuration}) =
        ratio2 * recip (minDurationWidth * (1 + magic * logBase 2 ratio1))
      where
        ratio1 = doubleFromDuration (smallestDuration / minDuration)
        ratio2 = doubleFromDuration (smallestDuration / springDuration)

data SpringWithRod = SWR
    { rodLength :: Double
    , springConst :: Double
    }
    deriving (Show, Eq)

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
        -- f = (x - xMin') / c
        f = (x - xMin') * c

        preStretchForce SWR{rodLength, springConst} = rodLength * springConst