module Main where

import Data.Jianpu.Graphics.Slice (Slices (Slices), sliceMusic)
import Data.Jianpu.Graphics.Spacing
import Data.Jianpu.Syntax.Document
import Data.Jianpu.Syntax.Parser qualified as Parser
import GHC.IO.Encoding
import System.Environment (getArgs)
import System.Exit
import Text.Parsec (runParser)
import Data.Jianpu.Document.Abstract (abstractFromDocument)

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
              -- let Slices slices spanGroups = sliceMusic music
              -- let springConstants = computeSpringConstants slices
              -- -- print slices
              -- -- print springConstants
              -- print spanGroups
              -- -- let neighbours = groupByNeighbours slices
              -- -- for_ neighbours $ \neighbour -> do
              -- --     putStrLn ""
              -- --     traverse print neighbour
              -- exitSuccess
    _ -> do
      putStrLn "Please provide a .yjp file."
      exitFailure
