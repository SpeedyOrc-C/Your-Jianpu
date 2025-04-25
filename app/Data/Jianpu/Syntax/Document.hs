module Data.Jianpu.Syntax.Document where

import Control.Monad.Reader (MonadTrans (lift))
import Data.Jianpu.Abstract.Error
import Data.Jianpu.Document
import Data.Jianpu.Document.Beaming (addBeams)
import Data.Jianpu.Graphics.Config (RenderContextT)
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

documentFromDraftMusic' ::
    DraftMusic -> RenderContextT HasError DocumentMusic
documentFromDraftMusic' draftMusic = do
    let (lexemesGroups, lyricsLinesGroups) = unzip draftMusic

    spansAndEventsGroups <- lift $ traverse extractTagSpans lexemesGroups

    let spans = fst <$> spansAndEventsGroups
    let entitiesGroups = calculateDurations <$> spansAndEventsGroups

    let DMusic voices = DMusic $ zipWith3 DVoice entitiesGroups spans lyricsLinesGroups

    voicesWithBeams <- traverse addBeams voices

    pure $ DMusic voicesWithBeams
