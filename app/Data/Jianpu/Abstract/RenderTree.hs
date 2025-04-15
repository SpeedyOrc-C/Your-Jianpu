module Data.Jianpu.Abstract.RenderTree (voicesTreeFromMusic) where

import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Data.Jianpu.Abstract (Music)
import Data.Jianpu.Graphics (Slice (elements), Slices (..))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap, lineWidth), RenderContextT, fill)
import Data.Jianpu.Graphics.Render (RenderObject, drawSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (..), computeSpringConstants, sff)
import Data.Layout (HasBox (getBox), LayoutTree (..), boxBoundX, boxBoundY, moveDown, moveRight)
import Data.List (transpose)
import Data.List.NonEmpty qualified as NE
import Data.Jianpu.Abstract.Error (HasError)

voicesTreeFromMusic :: Music -> RenderContextT HasError (LayoutTree RenderObject)
voicesTreeFromMusic music = do
    let Slices slices spanGroups = sliceMusic music

    let slicesElementsBoxes' = mapM (mapM (drawSliceElement >=> (fill . getBox)) . elements) slices

    slicesElementsBoxes <- slicesElementsBoxes'

    let slicesElementsBoundsY = map (map boxBoundX) slicesElementsBoxes

    let springMinLengths =
            maximum (negativeSize <$> head slicesElementsBoundsY)
                : zipWith
                    ( \slice1 slice2 ->
                        maximum $
                            zipWith
                                (+)
                                (positiveSize <$> slice1)
                                (negativeSize <$> slice2)
                    )
                    slicesElementsBoundsY
                    (tail slicesElementsBoundsY)
                ++ [maximum (positiveSize <$> last slicesElementsBoundsY)]

    let springConstants = (1 / 0) : computeSpringConstants slices

    let springs = zipWith SWR springMinLengths springConstants

    lineWidth <- asks lineWidth

    let force = sff (NE.sort (NE.fromList springs)) lineWidth

    let springExtensions =
            [max rodLength (force / springConst) | SWR{..} <- springs]

    let voicesBoxes = transpose slicesElementsBoxes

    let voicesBoundsY = boxBoundY . mconcat <$> voicesBoxes

    slicesElements <- mapM (mapM drawSliceElement . elements) slices

    let voicesElements = transpose slicesElements

    let slicesOffsetX = init $ scanl1 (+) springExtensions

    lineGap <- asks lineGap

    let voicesOffsetsY = scanl1 (+) $ do
            negativeSize (head voicesBoundsY)
                : zipWith
                    (\bottom top -> bottom + lineGap + top)
                    (positiveSize <$> voicesBoundsY)
                    (negativeSize <$> tail voicesBoundsY)

    pure $ LTNode mempty $ do
        (voiceElements, voiceOffsetY) <- zip voicesElements voicesOffsetsY
        pure $ LTNode (moveDown voiceOffsetY) $ do
            (voiceElement, sliceOffsetX) <- zip voiceElements slicesOffsetX
            pure $ LTNode (moveRight sliceOffsetX) [voiceElement]

negativeSize :: (Double, Double) -> Double
negativeSize (x, _) = max 0 (-x)

positiveSize :: (Double, Double) -> Double
positiveSize (_, x) = max 0 x