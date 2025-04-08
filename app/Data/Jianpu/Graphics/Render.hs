module Data.Jianpu.Graphics.Render where

import Control.Monad.Reader
import Data.Jianpu.Graphics.Config
import Data.Jianpu.Types
import Data.Layout
import Data.Maybe

data RenderObject
    = Glyph Glyph
    | GAccidental GAccidental
    | Circle Double
    | Rectangle Double Double
    deriving (Show)

data Glyph = GX | G0 | G1 | G2 | G3 | G4 | G5 | G6 | G7 | GRepeater4
    deriving (Show)

data GAccidental = GNatural | GSharp | GFlat | GDoubleSharp | GDoubleFlat
    deriving (Show)

instance HasSize RenderConfig RenderObject where
    getSize :: RenderObject -> Reader RenderConfig BoundingBox
    getSize Glyph{} = do
        glyphHeight <- asks getGlyphHeight
        glyphWidth <- asks getGlyphWidth
        pure $ BBox ((0, 0), (glyphWidth, glyphHeight))
    getSize _ = undefined

drawRepeater4 :: LayoutTree RenderObject
drawRepeater4 = LTLeaf APBottom $ Glyph GRepeater4

drawWhiteKey :: WhiteKey -> RenderObject
drawWhiteKey k = Glyph $ case k of
    K1 -> G1
    K2 -> G2
    K3 -> G3
    K4 -> G4
    K5 -> G5
    K6 -> G6
    K7 -> G7

drawAccidental :: Accidental -> RenderObject
drawAccidental a = GAccidental $ case a of
    Natural -> GNatural
    Sharp -> GSharp
    Flat -> GFlat
    DoubleSharp -> GDoubleSharp
    DoubleFlat -> GDoubleFlat

drawPitch ::
    Pitch ->
    TimeMultiplier ->
    Reader RenderConfig (LayoutTree RenderObject)
drawPitch Pitch{..} timeMultiplier = do
    beamHeight <- asks getBeamHeight
    beamGap <- asks getBeamGap
    transposeDotGap <- asks getTransposeDotGap
    transposeDotRadius <- asks getTransposeDotRadius
    glyphHeight <- asks getGlyphHeight
    glyphWidth <- asks getGlyphWidth

    pure . LTNode mempty $
        [ LTNode
            (moveUp $ glyphHeight + transposeDotGap)
            [ LTNode
                (moveDown $ (i - 1) * (transposeDotGap + 2 * transposeDotRadius))
                [LTLeaf APBottom (Circle transposeDotRadius)]
            | i <- fromIntegral <$> [1 .. octaveTranspose]
            ]
        , LTNode
            (move (-(glyphWidth / 2)) (-glyphHeight))
            (maybeToList (LTLeaf APTopRight . drawAccidental <$> accidental))
        , LTLeaf
            APBottom
            (drawWhiteKey whiteKey)
        , LTNode
            (moveDown $ (beamGap + beamHeight) * fromIntegral (fromEnum timeMultiplier) + transposeDotGap)
            [ LTNode
                (moveDown $ (i - 1) * (transposeDotGap + 2 * transposeDotRadius))
                [LTLeaf APTop (Circle transposeDotRadius)]
            | i <- fromIntegral <$> [1 .. (-octaveTranspose)]
            ]
        ]
