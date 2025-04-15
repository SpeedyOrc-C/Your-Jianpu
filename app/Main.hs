module Main where

import Control.Monad.Reader (runReader)
import Control.Monad.Writer (Endo (appEndo), execWriter)
import Data.Foldable (for_)
import Data.Jianpu.Document.Abstract (abstractFromDocument)
import Data.Jianpu.Graphics.Config (defaultRenderConfig)
import Data.Jianpu.Graphics.SVG (putDrawDirective, putSvgEnd, putSvgPrelude)
import Data.Jianpu.Syntax.Document (documentFromDraftMusic)
import Data.Jianpu.Syntax.Parser qualified as Parser
import Data.Layout (BoundingBox (..), HasBox (getBox), flattenLayoutTree)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_Your_Jianpu_Renderer (getDataDir)
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (splitExtension)
import Text.Parsec (runParser)
import Data.Jianpu.Abstract.RenderTree (renderMusic)

main :: IO ()
main = do
  -- Make Windows happy!
  setLocaleEncoding utf8

  getArgs >>= \case
    [inputPath] -> do
      input <- readFile inputPath
      main_ inputPath input
    _ -> do
      putStrLn "Please provide a .yjp file."
      exitFailure

main_ inputPath input =
  case runParser Parser.pFile () inputPath input of
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
              let (inputPathNoExt, _) = splitExtension inputPath

              createDirectoryIfMissing True (inputPathNoExt ++ "/asset")

              writeFile (inputPathNoExt ++ "/index.svg") (generateOutputFrom music)

              dataDir <- getDataDir
              dataFiles <- listDirectory (dataDir ++ "/asset")

              for_ dataFiles $ \dataFile -> do
                copyFile
                  (dataDir ++ "/asset/" ++ dataFile)
                  (inputPathNoExt ++ "/asset/" ++ dataFile)

generateOutputFrom music = output
 where
  config = defaultRenderConfig

  voicesTrees = runReader (renderMusic music) config

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
