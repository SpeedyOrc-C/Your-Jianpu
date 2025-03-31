module Main where

import Data.Jianpu.Syntax.Parser qualified as Parser
import GHC.IO.Encoding
import System.Environment (getArgs)
import System.Exit
import Text.Parsec (runParser)
import Data.Jianpu.Abstract.Main (abstractMain)

main :: IO ()
main = do
    setLocaleEncoding utf8

    getArgs >>= \case
        [inputPath] -> do
            input <- readFile inputPath
            case runParser Parser.pMusic () inputPath input of
                Left parseError -> do
                    print parseError
                    exitFailure
                Right syntaxMusic -> do
                    case abstractMain syntaxMusic of
                        Left abstractErrors -> do
                            print abstractErrors
                            exitFailure
                        Right music -> do
                            print music
                            exitSuccess

        _ -> do
            putStrLn "Please provide a .yjp file."
            exitFailure
