module Data.Jianpu.Syntax.Document where

import Data.Jianpu.Abstract.Error
import Data.Jianpu.Document
import Data.Jianpu.Syntax
import Data.Jianpu.Syntax.CalculateDurations
import Data.Jianpu.Syntax.ExtractTagSpans

documentFromDraftMusic :: DraftMusic -> Either [AbstractError] DocumentMusic
documentFromDraftMusic draftMusic = do
    let (lexemesGroups, lyricsLinesGroups) = unzip draftMusic

    spansAndEventsGroups <- traverse extractTagSpans lexemesGroups

    let spans = fst <$> spansAndEventsGroups
    let entitiesGroups = calculateDurations <$> spansAndEventsGroups

    pure $ DMusic $ zipWith3 DVoice entitiesGroups spans lyricsLinesGroups
