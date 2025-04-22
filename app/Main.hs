module Main where

import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.Writer (Endo (appEndo), execWriter)
import Data.Foldable (for_, traverse_)
import Data.Jianpu.Abstract.RenderTree (engraveMusic)
import Data.Jianpu.Document.Abstract (abstractFromDocument)
import Data.Jianpu.Graphics.Config (defaultRenderConfig, fill)
import Data.Jianpu.Graphics.SVG (putDrawDirective, putSvgEnd, putSvgPrelude)
import Data.Jianpu.Syntax.Document (documentFromDraftMusic')
import Data.Jianpu.Syntax.Parser (markupToDraft)
import Data.Layout (BoundingBox (..), HasBox (getBox), flattenLayoutTree)
import Data.List.Utils (takeFirst)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_Your_Jianpu_Renderer (getDataDir)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeDirectory)

main :: IO ()
main = do
    -- Make Windows happy!
    setLocaleEncoding utf8

    args <- getArgs
    maybeDataDir <- getRealDataDir

    case args of
        ["-v"] -> do
            putStrLn "[Your Jianpu Renderer]"
            case maybeDataDir of
                Nothing -> do
                    putStrLn "No valid data directory found."
                    exitFailure
                Just dir -> do
                    putStrLn ("Data directory: " ++ dir)
                    exitSuccess
        [inputPath] -> do
            input <- readFile inputPath
            case scoreSvgFromMarkup defaultRenderConfig inputPath input of
                Left errors -> do
                    traverse_ print errors
                    exitFailure
                Right output -> do
                    -- Generate output
                    let outputDir = dropExtension inputPath
                    createDirectoryIfMissing True (outputDir ++ "/asset")
                    writeFile (outputDir ++ "/index.svg") output

                    case maybeDataDir of
                        Nothing -> do
                            putStrLn "No valid data directory found."
                            exitFailure
                        Just dataDir -> do
                            dataFiles <- listDirectory (dataDir ++ "/asset")
                            for_ dataFiles $ \dataFile -> do
                                copyFile
                                    (dataDir ++ "/asset/" ++ dataFile)
                                    (outputDir ++ "/asset/" ++ dataFile)
                            exitSuccess
        _ -> do
            putStrLn "Please provide a .yjp file."
            exitFailure

getRealDataDir :: IO (Maybe FilePath)
getRealDataDir = do
    possibleDirs <-
        sequence
            [ getCurrentDirectory
            , getDataDir
            , takeDirectory <$> getExecutablePath
            ]

    dirsExist <-
        traverse
            doesDirectoryExist
            [d ++ "/asset" | d <- possibleDirs]

    let dirFindResult = takeFirst fst (zip dirsExist possibleDirs)

    case dirFindResult of
        Nothing -> return Nothing
        Just ((_, dir), _) -> return (Just dir)

scoreSvgFromMarkup config inputPath markup = (`runReaderT` config) $ do
    draft <- lift $ markupToDraft inputPath markup
    document <- documentFromDraftMusic' draft
    music <- lift $ abstractFromDocument document
    scoreSvgFromMusic music

scoreSvgFromMusic music = do
    voicesTrees <- engraveMusic music

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
