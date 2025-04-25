module Data.Jianpu.Document.Abstract where
import Data.Jianpu.Document
import Data.Jianpu.Abstract
import Data.Jianpu.Abstract.GenerateLyricsVoices
import Data.Jianpu.Abstract.Error (AbstractError)

abstractFromDocument :: DocumentMusic -> Either [AbstractError] Music
abstractFromDocument (DMusic syntaxVoices) = do
    let entitiesGroups = dEntities <$> syntaxVoices
    let spansGroups = dSpans <$> syntaxVoices
    let normalVoices = zipWith Voice entitiesGroups spansGroups

    lyricsLinesGroups <- generateLyricsVoices (lyrics <$> syntaxVoices) entitiesGroups

    let lyricsVoicesGroups = fmap (fmap (`Voice` mempty)) lyricsLinesGroups

    pure $ Music $ concat (zipWith (:) normalVoices lyricsVoicesGroups)
