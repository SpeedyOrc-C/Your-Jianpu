module Main where

import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.Writer (Endo (appEndo), execWriter)
import Data.Foldable (for_, traverse_)
import Data.Jianpu.Abstract.RenderTree (voicesTreeFromMusic)
import Data.Jianpu.Document.Abstract (abstractFromDocument)
import Data.Jianpu.Graphics.Config (defaultRenderConfig, fill)
import Data.Jianpu.Graphics.SVG (putDrawDirective, putSvgEnd, putSvgPrelude)
import Data.Jianpu.Syntax.Document (documentFromDraftMusic')
import Data.Jianpu.Syntax.Parser (markupToDraft)
import Data.Layout (BoundingBox (..), HasBox (getBox), flattenLayoutTree)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_Your_Jianpu_Renderer (getDataDir)
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (splitExtension)

main :: IO ()
main = do
  -- Make Windows happy!
  setLocaleEncoding utf8

  args <- getArgs

  case args of
    [inputPath] -> do
      input <- readFile inputPath
      case scoreSvgFromMarkup defaultRenderConfig inputPath input of
        Left errors -> do
          traverse_ print errors
          exitFailure
        Right output -> do
          let (inputPathNoExt, _) = splitExtension inputPath

          createDirectoryIfMissing True (inputPathNoExt ++ "/asset")

          writeFile (inputPathNoExt ++ "/index.svg") output

          dataDir <- getDataDir
          dataFiles <- listDirectory (dataDir ++ "/asset")

          for_ dataFiles $ \dataFile -> do
            copyFile
              (dataDir ++ "/asset/" ++ dataFile)
              (inputPathNoExt ++ "/asset/" ++ dataFile)
    _ -> do
      putStrLn "Please provide a .yjp file."
      exitFailure

scoreSvgFromMarkup config inputPath markup = (`runReaderT` config) $ do
  draft <- lift $ markupToDraft inputPath markup
  document <- documentFromDraftMusic' draft
  music <- lift $ abstractFromDocument document
  scoreSvgFromMusic music

scoreSvgFromMusic music = do
  voicesTrees <- voicesTreeFromMusic music

  let flattenedTree = flattenLayoutTree voicesTrees

  box <- fill $ getBox voicesTrees

  let height = case box of
        NoBox -> 0
        BBox ((_, y1), (_, y2)) -> y2 - y1

  preludeWriter <- fill $ putSvgPrelude height
  bodyWriters <- fill $ traverse putDrawDirective flattenedTree
  endWriter <- fill putSvgEnd

  let outputEndoWriter = do
        preludeWriter
        sequence_ bodyWriters
        endWriter
  let outputEndo = execWriter outputEndoWriter
  let output = appEndo outputEndo ""

  pure output
