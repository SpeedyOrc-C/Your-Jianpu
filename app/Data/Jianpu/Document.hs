module Data.Jianpu.Document where

import Data.Jianpu.Abstract (Entity, Spans)
import Data.Jianpu.Types (Syllable)

newtype DocumentMusic = DMusic [DocumentVoice]

data DocumentVoice
    = DVoice
    { dEntities :: [Entity]
    , dSpans :: Spans
    , lyrics :: [[Maybe Syllable]]
    }
