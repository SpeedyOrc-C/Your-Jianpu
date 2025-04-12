module Main where

import Control.Monad ((>=>))
import Control.Monad.Reader (runReader)
import Data.Jianpu.Document.Abstract (abstractFromDocument)
import Data.Jianpu.Graphics (Slice (elements), Slices (Slices))
import Data.Jianpu.Graphics.Config (defaultRenderConfig)
import Data.Jianpu.Graphics.Render (drawSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (SWR), computeSpringConstants, sff)
import Data.Jianpu.Syntax.Document (documentFromDraftMusic)
import Data.Jianpu.Syntax.Parser qualified as Parser
import Data.Layout (BoundingBox (..), HasBox (getBox))
import Data.List.NonEmpty qualified as NE
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Parsec (runParser)

main :: IO ()
main = do
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
                  print slicesElementsYBounds
                  print springs
                  print minTotalLength
                  print force
                  exitSuccess
                 where
                  config = defaultRenderConfig

                  Slices slices spanGroups = sliceMusic music

                  slicesElementsBoxes' = mapM (mapM (drawSliceElement >=> getBox) . elements) slices
                  slicesElementsBoxes = runReader slicesElementsBoxes' config

                  getXBounds :: BoundingBox -> (Double, Double)
                  getXBounds NoBox = (0, 0)
                  getXBounds (BBox ((x1, _), (x2, _))) = (x1, x2)

                  getLeftSize :: (Double, Double) -> Double
                  getLeftSize (x, _) = max 0 (-x)

                  getRightSize :: (Double, Double) -> Double
                  getRightSize (_, x) = max 0 x

                  slicesElementsYBounds = map (map getXBounds) slicesElementsBoxes

                  springMinLengths =
                    maximum (getLeftSize <$> head slicesElementsYBounds)
                      : zipWith
                        ( \slice1 slice2 ->
                            maximum $
                              zipWith
                                (+)
                                (getRightSize <$> slice1)
                                (getLeftSize <$> slice2)
                        )
                        slicesElementsYBounds
                        (tail slicesElementsYBounds)
                      ++ [maximum (getRightSize <$> last slicesElementsYBounds)]

                  springConstants = (1 / 0) : computeSpringConstants slices

                  springs = zipWith SWR springMinLengths springConstants

                  minTotalLength = sum springMinLengths

                  force = sff (NE.fromList springs) 150
    [inputPath, configPath] -> do
      exitSuccess
    _ -> do
      putStrLn "Please provide a .yjp file."
      exitFailure
