module Data.Jianpu.Graphics.Spacing where

import Data.Jianpu.Abstract.Types
import Data.Jianpu.Graphics.Slice
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio

data SpringWithRod = SWR
    { rodLength :: Double
    , springConst :: Double
    }
    deriving (Show, Eq)

computeSpringConsts :: MusicSlices -> [Double]
computeSpringConsts =
    undefined
  where
    computeSpringConsts' :: MusicSlices -> MusicSlice -> Maybe Double
    -- Since shorter notes are harder to stretch, and tags have no duration,
    -- the spring constant will be infinite.
    -- Actually, they can't be stretched at all.
    -- So we'll use
    computeSpringConsts' _ (0, _) = Nothing

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
