module Data.Jianpu.Graphics.TextMetric where

import Control.Monad
import Data.List.Utils
import System.Directory
import System.Exit
import System.Process

data Request = Request
    { fontSize :: Int
    , font :: String
    , strings :: [String]
    }
    deriving (Show)

newtype Response = Response (Maybe [(Double, Double)])
    deriving (Show)

fetch :: Request -> IO Response
fetch (Request{fontSize, font, strings}) =
    withCurrentDirectory "./text-metric" $ do
        (ret, out, err) <-
            readProcessWithExitCode
                "./index.js"
                (show fontSize : font : strings)
                ""

        return . Response $
            if ret /= ExitSuccess || not (null err)
                then
                    Nothing
                else forM (splitOn ';' out) $ \item ->
                    case splitOn '*' item of
                        [read -> width, read -> height] -> Just (width, height)
                        _ -> Nothing
