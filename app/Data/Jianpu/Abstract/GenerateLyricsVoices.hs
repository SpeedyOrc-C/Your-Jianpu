module Data.Jianpu.Abstract.GenerateLyricsVoices where

import Control.Monad (zipWithM)
import Data.Jianpu.Abstract (Entity (Event, duration))
import Data.Jianpu.Abstract.Error (AbstractError (..))
import Data.Jianpu.Types (Event (Pronounce), Syllable)
import Data.Maybe (mapMaybe)

{-
Generate the boilerplate lyrics voices directly from the melody voice and
their notes' durations.
-}
generateLyricsVoices :: [[[Maybe Syllable]]] -> [[Entity]] -> Either [AbstractError] [[[Entity]]]
generateLyricsVoices lyricsLinesGroups entitiesGroups =
    zipWithM generateLyricsVoices' entitiesGroups lyricsLinesGroups

generateLyricsVoices' :: [Entity] -> [[Maybe Syllable]] -> Either [AbstractError] [[Entity]]
generateLyricsVoices' voice lyricsLines =
    if all ((== length durations) . length) lyricsLines
        then Right $ (\line -> zipWith (Event . Pronounce) line durations) <$> lyricsLines
        else Left [InconsistentSyllableCount (length durations)]
  where
    durations = flip mapMaybe voice $ \case
        Event{duration} -> Just duration
        _ -> Nothing
