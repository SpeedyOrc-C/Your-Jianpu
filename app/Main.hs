module Main where

import Control.Monad ((>=>))
import Control.Monad.Reader (asks, runReader)
import Data.Jianpu.Document.Abstract (abstractFromDocument)
import Data.Jianpu.Graphics (Slice (elements), Slices (Slices))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap), defaultRenderConfig)
import Data.Jianpu.Graphics.Render (drawSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (SWR, rodLength, springConst), computeSpringConstants, sff)
import Data.Jianpu.Syntax.Document (documentFromDraftMusic)
import Data.Jianpu.Syntax.Parser qualified as Parser
import Data.Layout (BoundingBox (..), HasBox (getBox), LayoutTree (LTNode), boxBoundX, boxBoundY, moveDown, moveRight, flattenLayoutTree)
import Data.List (transpose)
import Data.List.NonEmpty qualified as NE
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Parsec (runParser)
import Data.Jianpu.Graphics.SVG (putSvgPrelude, putDrawDirective, putSvgEnd)
import Control.Monad.Writer (execWriter, Endo (appEndo))

main :: IO ()
main = do
  -- Make Windows happy!
  setLocaleEncoding utf8

  getArgs >>= \case
    [inputPath] -> do
      input <- readFile inputPath
      case runParser Parser.pMusic () inputPath input of
        Left errors -> do
          print errors
          exitFailure
        Right draftMusic -> do
          case documentFromDraftMusic draftMusic of
            Left errors -> do
              print errors
              exitFailure
            Right document -> do
              case abstractFromDocument document of
                Left errors -> do
                  print errors
                  exitFailure
                Right music -> do
                  print slicesOffsetX
                  print voicesBoundsY
                  print voicesOffsetsY
                  print voicesTrees
                  writeFile "./svg-output/index.svg" output
                 where
                  config = defaultRenderConfig

                  Slices slices spanGroups = sliceMusic music

                  slicesElementsBoxes' = mapM (mapM (drawSliceElement >=> getBox) . elements) slices
                  slicesElementsBoxes = runReader slicesElementsBoxes' config

                  negativeSize :: (Double, Double) -> Double
                  negativeSize (x, _) = max 0 (-x)

                  positiveSize :: (Double, Double) -> Double
                  positiveSize (_, x) = max 0 x

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

                  minTotalLength = sum springMinLengths

                  force = sff (NE.fromList springs) (1000 - minTotalLength)

                  springExtensions =
                    [rodLength + force / springConst | SWR{..} <- springs]

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

                  voicesTrees = LTNode mempty $ do
                    (voiceElements, voiceOffsetY) <- zip voicesElements voicesOffsetsY
                    pure $ LTNode (moveDown voiceOffsetY) $ do
                      (voiceElement, sliceOffsetX) <- zip voiceElements slicesOffsetX
                      pure $ LTNode (moveRight sliceOffsetX) [voiceElement]

                  flattenedTree = flattenLayoutTree voicesTrees

                  outputEndoWriter = (`runReader` config) $ do
                    box <- getBox voicesTrees
                    let height = case box of
                          NoBox -> 0
                          BBox ((_, y1), (_, y2)) -> y2 - y1

                    preludeWriter <- putSvgPrelude height
                    bodyWriters <- traverse putDrawDirective flattenedTree
                    endWriter <- putSvgEnd

                    pure $ do
                      preludeWriter
                      sequence_ bodyWriters
                      endWriter

                  outputEndo = execWriter outputEndoWriter

                  output = appEndo outputEndo ""

    [inputPath, configPath] -> do
      exitSuccess
    _ -> do
      putStrLn "Please provide a .yjp file."
      exitFailure
