module Data.Jianpu.Syntax.ExtractTagSpans (extractTagSpans) where

import Control.Monad.State (
    MonadState (state),
    State,
    modify,
    runState,
 )
import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Jianpu.Abstract.Error (
    AbstractError (
        ErrorArgsBeam,
        ErrorArgsFermata,
        ErrorArgsSlur,
        ErrorArgsTie,
        ErrorArgsTimeSignature,
        ErrorArgsTuplet,
        TagMismatch,
        UnknownTag
    ),
 )
import Data.Jianpu.Abstract (
    Span (Beam, Fermata, Slur, Tie, Tuplet),
    Tag (..), Spans, Interval (I),
 )
import Data.Jianpu.Syntax ( Argument(AInt), Lexeme(..) )
import Data.Jianpu.Types (Event)
import Data.List.Utils (takeFirst)
import Data.Maybe (mapMaybe)

data ExtractorState = ES
    { currentIndex :: Int
    , currentSpans :: Spans
    , tagStartStack :: [TagStartEntry]
    }
    deriving (Show)

type TagStartEntry =
    ( Int -- position
    , String -- name
    , Maybe Int -- ID
    , [Argument]
    )

extractTagSpans :: [Lexeme] -> Either [AbstractError] (Spans, [Either Tag Event])
extractTagSpans entities' =
    case errors of
        [] -> Right (tagSpans, entities)
        _ -> Left errors
  where
    ES{currentSpans = tagSpans} = s

    entities = flip mapMaybe xs $ \case
        Right (Just entity) -> Just entity
        _ -> Nothing

    errors = flip mapMaybe xs $ \case
        Left error' -> Just error'
        _ -> Nothing

    initialState = ES{currentIndex = 0, tagStartStack = [], currentSpans = IM.empty}

    (xs, s) = runState (traverse extractOne entities') initialState

extractOne :: Lexeme -> State ExtractorState (Either AbstractError (Maybe (Either Tag Event)))
extractOne ColonSingleBarColon =
    (Right . Just $ Left BeginEndRepeat) <$ oneStepForward
extractOne SingleBarColon =
    (Right . Just $ Left BeginRepeat) <$ oneStepForward
extractOne ColonSingleBar =
    (Right . Just $ Left EndRepeat) <$ oneStepForward
extractOne DoubleBar =
    (Right . Just $ Left DoubleBarLine) <$ oneStepForward
extractOne TripleBar =
    (Right . Just $ Left EndSign) <$ oneStepForward
extractOne SingleBar =
    (Right . Just $ Left BarLine) <$ oneStepForward
extractOne (LEvent event) =
    (Right . Just $ Right event) <$ oneStepForward
extractOne (Tag0 "signature" args) =
    case args of
        [AInt a, AInt b] ->
            (Right . Just $ Left $ TimeSignature a b) <$ oneStepForward
        _ -> return $ Left ErrorArgsTimeSignature
extractOne (Tag0 name _) = pure . Left $ UnknownTag name
extractOne (Tag1 name args) =
    case extractTagSpan name args of
        Left error' -> return $ Left error'
        Right tagSpan ->
            state $ \es@ES{currentIndex, currentSpans} ->
                ( return Nothing
                , es
                    { currentIndex
                    , currentSpans =
                        IM.insert
                            (I (currentIndex, currentIndex))
                            tagSpan
                            currentSpans
                    }
                )
extractOne (TagStart name idx args) =
    state $ \es@ES{currentIndex, tagStartStack} ->
        ( Right Nothing
        , es{tagStartStack = (currentIndex, name, idx, args) : tagStartStack}
        )
extractOne (TagEnd name idx) = state $ \es@ES{currentIndex, currentSpans, tagStartStack} -> do
    let thatMatchesThis :: TagStartEntry -> Bool
        thatMatchesThis (_, name', idx', _) = tagMatched (name', idx') (name, idx)

    case takeFirst thatMatchesThis tagStartStack of
        Nothing -> (Left TagMismatch, es)
        Just ((startIndex, tagName, _, args), tagStartStack') ->
            case extractTagSpan tagName args of
                Left error' ->
                    ( Left error'
                    , es{tagStartStack = tagStartStack'}
                    )
                Right tagSpan ->
                    ( Right Nothing
                    , es
                        { currentSpans =
                            IM.insert
                                (I (startIndex, currentIndex - 1))
                                tagSpan
                                currentSpans
                        , tagStartStack = tagStartStack'
                        }
                    )

oneStepForward :: State ExtractorState ()
oneStepForward = modify $ \es@ES{currentIndex} -> es{currentIndex = currentIndex + 1}

tagMatched :: (String, Maybe Int) -> (Maybe String, Maybe Int) -> Bool
tagMatched _ (Nothing, Nothing) = True
tagMatched (_, Just idx) (Nothing, Just idx') = idx == idx'
tagMatched (name, _) (Just name', Nothing) = name == name'
tagMatched (name, Just idx) (Just name', Just idx') = name == name' && idx == idx'
tagMatched _ _ = False

extractTagSpan :: String -> [Argument] -> Either AbstractError Span
extractTagSpan "tuplet" = \case
    [AInt count] -> Right $ Tuplet count
    _ -> Left ErrorArgsTuplet
extractTagSpan "fermata" = extractArg0 Fermata ErrorArgsFermata
extractTagSpan "beam" = extractArg0 Beam ErrorArgsBeam
extractTagSpan "tie" = extractArg0 Tie ErrorArgsTie
extractTagSpan "slur" = extractArg0 Slur ErrorArgsSlur
extractTagSpan tag = const . Left $ UnknownTag tag

extractArg0 :: Span -> AbstractError -> [Argument] -> Either AbstractError Span
extractArg0 constructor error' = \case
    [] -> Right constructor
    _ -> Left error'
