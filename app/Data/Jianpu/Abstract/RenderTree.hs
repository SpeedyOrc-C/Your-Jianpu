module Data.Jianpu.Abstract.RenderTree where

import Control.Monad ((>=>))
import Control.Monad.Reader (asks, runReader)
import Data.Jianpu.Abstract (Music)
import Data.Jianpu.Graphics (Slice (elements), Slices (..))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap))
import Data.Jianpu.Graphics.Render (RenderObject, drawSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (..), computeSpringConstants, sff)
import Data.Layout (HasBox (getBox), LayoutTree (..), boxBoundX, boxBoundY, moveDown, moveRight)
import Data.List (transpose)
import Data.List.NonEmpty qualified as NE

renderMusic :: RenderConfig -> Music -> LayoutTree RenderObject
renderMusic config music =
    LTNode mempty $ do
        (voiceElements, voiceOffsetY) <- zip voicesElements voicesOffsetsY
        pure $ LTNode (moveDown voiceOffsetY) $ do
            (voiceElement, sliceOffsetX) <- zip voiceElements slicesOffsetX
            pure $ LTNode (moveRight sliceOffsetX) [voiceElement]
  where
    Slices slices spanGroups = sliceMusic music

    slicesElementsBoxes' = mapM (mapM (drawSliceElement >=> getBox) . elements) slices
    slicesElementsBoxes = runReader slicesElementsBoxes' config

    slicesElementsBoundsY = map (map boxBoundX) slicesElementsBoxes

    springMinLengths =
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

    springConstants = (1 / 0) : computeSpringConstants slices

    springs = zipWith SWR springMinLengths springConstants

    force = sff (NE.sort (NE.fromList springs)) 2000

    springExtensions =
        [max rodLength (force / springConst) | SWR{..} <- springs]

    voicesBoxes = transpose slicesElementsBoxes

    voicesBoundsY = boxBoundY . mconcat <$> voicesBoxes

    slicesElements' = mapM (mapM drawSliceElement . elements) slices
    slicesElements = runReader slicesElements' config

    voicesElements = transpose slicesElements

    slicesOffsetX = init $ scanl1 (+) springExtensions

    voicesOffsetsY = scanl1 (+) . (`runReader` config) $ do
        lineGap <- asks lineGap
        pure $
            negativeSize (head voicesBoundsY)
                : zipWith
                    (\bottom top -> bottom + lineGap + top)
                    (positiveSize <$> voicesBoundsY)
                    (negativeSize <$> tail voicesBoundsY)

negativeSize :: (Double, Double) -> Double
negativeSize (x, _) = max 0 (-x)

positiveSize :: (Double, Double) -> Double
positiveSize (_, x) = max 0 x