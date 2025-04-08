module Data.Jianpu.Graphics.Config where

import Data.List.NonEmpty (NonEmpty (..))

data RenderConfig = RCfg
    { lineWidth :: Double
    , glyphHeight'lineWidth :: Double
    , glyphWidth'glyphHeight :: Double
    , transposeDotRadius'glyphHeight :: Double
    , transposeDotGap'glyphHeight :: Double
    , beamHeight'glyphHeight :: Double
    , beamGap'glyphHeight :: Double
    , accidentalHeight'glyphHeight :: Double
    , accidentalWidth'accidentalHeight :: Double
    }

getGlyphHeight :: RenderConfig -> Double
getGlyphWidth :: RenderConfig -> Double
getTransposeDotRadius :: RenderConfig -> Double
getTransposeDotGap :: RenderConfig -> Double
getBeamHeight :: RenderConfig -> Double
getBeamGap :: RenderConfig -> Double
getGlyphHeight RCfg{..} = glyphHeight'lineWidth * lineWidth
getGlyphWidth cfg@RCfg{..} = glyphWidth'glyphHeight * getGlyphHeight cfg
getTransposeDotRadius cfg@RCfg{..} = transposeDotRadius'glyphHeight * getGlyphHeight cfg
getTransposeDotGap cfg@RCfg{..} = transposeDotGap'glyphHeight * getGlyphHeight cfg
getBeamHeight cfg@RCfg{..} = beamHeight'glyphHeight * getGlyphHeight cfg
getBeamGap cfg@RCfg{..} = beamGap'glyphHeight * getGlyphHeight cfg

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RCfg
        { lineWidth = 1000
        -- Names with an apostrophe are ratios.
        -- e.g. abc'def has the value abc / def.
        , glyphHeight'lineWidth = 1 / 20
        , glyphWidth'glyphHeight = 3 / 4
        , transposeDotRadius'glyphHeight = 1 / 20
        , transposeDotGap'glyphHeight = 1 / 20
        , beamHeight'glyphHeight = 1 / 30
        , beamGap'glyphHeight = 1 / 30
        , accidentalHeight'glyphHeight = 7 / 10
        , accidentalWidth'accidentalHeight = 1 / 2
        }

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
