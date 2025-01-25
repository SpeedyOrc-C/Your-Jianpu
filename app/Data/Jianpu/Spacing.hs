module Data.Jianpu.Spacing where

import Data.List.NonEmpty (NonEmpty ((:|)))

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
        f = (x - xMin') / c

        preStretchForce SWR{rodLength, springConst} = rodLength * springConst
