{-# LANGUAGE MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , UndecidableInstances
  , FunctionalDependencies
  , TypeSynonymInstances
  #-}
{-| Provides the ability to get an execution trace for a monadic computation
 -}
module Control.Monad.Trace
    ( MonadTrace(..)
    , TraceString(..)
    , log
    , frame

    -- * Add tracing to a monad
    , TraceT
    , runTraceT
    , trace

    -- ** The "Trace" type
    , Trace(..)
    , ppTrace

    -- * Trace throws and catches in a MonadError
    , MonadTracedErrors(..)
    , TracedErrorT(..)
    , EmbedsErrors(..)
    , EmbedsMPlus(..)

    -- * A basic Trace message
    , TraceEvent(..)
    , ppTraceEvent

    -- * Ignore tracing
    , NoTraceT
    , Untraced
    , runNoTraceT
    , ignoreTracing
    )
where
import Prelude hiding ( log )
import Control.Monad.State ( StateT(..), modify, get, put, MonadState )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Applicative
import Control.Monad.Trans
import Control.Monad ( MonadPlus(..), ap )
import Control.Monad.Error ( MonadError (..), ErrorT(..), Error )
import Control.Monad.Reader ( MonadReader (..), ReaderT(..) )
import Control.Monad.Writer ( MonadWriter (..) )
import Control.Monad.RWS ( RWST(..) )
import Control.Monad.Random ( MonadRandom(..) )
import Data.Monoid ( Monoid )
import Text.PrettyPrint.HughesPJ as PP

-- | Computations that have the ability to provide tracing
class Monad m => MonadTrace msg m | m -> msg where
    -- |Trace that a particular action occurred with a message
    logMessage :: msg -> m ()
    logMessage msg = frameMessage msg $ return ()

    -- |Group log messages for this action together with this message
    frameMessage :: msg -> m a -> m a

    -- |Ignore any tracing that would have happened in this action
    untraced :: m a -> m a

class TraceString msg where
    traceString :: String -> msg

log :: (TraceString msg, MonadTrace msg m) => String -> m ()
log = logMessage . traceString

frame :: (TraceString msg, MonadTrace msg m) => String -> m a -> m a
frame = frameMessage . traceString

-- |The trace of an action
data Trace msg =
    Trace { message :: msg -- ^The message for this action
          , traceInfo :: [Trace msg]
          -- ^Trace information for any sub-computations
          } deriving Show

data Stack msg =
    Stack { _topFrame :: [Trace msg]
          , _currentFrame :: [Trace msg]
          }

frameDone :: Stack msg -> Stack msg
frameDone (Stack _ []) = Prelude.error "popping from an empty trace stack"
frameDone (Stack tl [Trace msg l]) = Stack (Trace msg (reverse l):tl) []
frameDone (Stack tl ((Trace msg1 l1):(Trace msg2 l2):ts)) =
    Stack tl $ Trace msg2 (Trace msg1 (reverse l1):l2):ts

newFrame :: msg -> Stack msg -> Stack msg
newFrame msg (Stack tl ts) = Stack tl $ Trace msg []:ts

-- |Transformer that adds tracing to a monad
newtype Monad m => TraceT msg m a = TraceT (StateT (Stack msg) m a)
    deriving ( Monad
             , MonadPlus
             , MonadError e
             , MonadWriter w
             , MonadTrans
             , MonadIO
             , Functor
             )

instance Monad m => MonadTrace msg (TraceT msg m) where
    frameMessage msg (TraceT act) =
        TraceT $ do
          modify $ newFrame msg
          val <- act
          modify frameDone
          return val

    untraced (TraceT act) =
        TraceT $ do
          originalStack <- get
          val <- act
          put originalStack
          return val

instance MonadState st m => MonadState st (TraceT msg m) where
    get = lift get
    put = lift . put

instance (Monad m, Applicative m) => Applicative (TraceT msg m) where
    pure = return
    (<*>) = ap

instance (Error e, MonadTrace msg m) => MonadTrace msg (ErrorT e m) where
    frameMessage msg act = ErrorT $ frameMessage msg $ runErrorT act
    untraced act = ErrorT $ untraced $ runErrorT act

-- |Evaluate a computation and return its result along with an execution trace
runTraceT :: Monad m => TraceT msg m a -> m (a, [Trace msg])
runTraceT (TraceT act) = do (x, (Stack tl _)) <- runStateT act $ Stack [] []
                            return (x, reverse tl)

-- |A computation that produces a result along with an execution trace
type Traced msg a = TraceT msg Identity a

-- |Run a computation and return its result along with an execution trace
trace :: Traced msg a -> (a, [Trace msg])
trace = runIdentity . runTraceT

-- |Pretty-print an execution trace
ppTrace :: (msg -> Doc) -> [Trace msg] -> Doc
ppTrace ppMsg = vcat . map pp
    where pp (Trace msg ts) =
              let subitems = (nest 4 $ vcat $ map pp ts)
              in ppMsg msg $+$ if null ts then PP.empty else subitems


--------------------------------------------------
-- Basic trace event type

data TraceEvent e msg =
    Throw e | Catch e | CatchStart | CatchEnd | MZero | MPlus | Msg msg

instance EmbedsErrors (TraceEvent e msg) e where
    embedThrow = Throw
    embedCatch = Catch
    embedCatchStart = CatchStart
    embedCatchEnd = CatchEnd

instance EmbedsMPlus (TraceEvent e msg) where
    embedMzero = MZero
    embedMplusFailure = MPlus

instance TraceString (TraceEvent e String) where
    traceString = Msg

ppTraceEvent :: (e -> Doc) -> (m -> Doc) -> TraceEvent e m -> Doc
ppTraceEvent ppe _ (Throw e) = text "< Throw" <+> ppe e
ppTraceEvent ppe _ (Catch e) = text "> Catch" <+> ppe e
ppTraceEvent _ _ MZero = text "< mzero"
ppTraceEvent _ _ MPlus = text "> mplus"
ppTraceEvent _ _ CatchStart = text "{"
ppTraceEvent _ _ CatchEnd = text "}"
ppTraceEvent _ ppm (Msg m) = text "." <+> ppm m

--------------------------------------------------
-- Trace errors

class MonadTracedErrors msg e m | m -> e, m -> msg where
    quietCatchError :: m a -> (e -> m a) -> m a
    quietThrowError :: e -> m a

instance MonadTracedErrors msg e m =>
    MonadTracedErrors msg e (StateT st m) where
        a `quietCatchError` handler = StateT $ \st -> runStateT a st
                                      `quietCatchError` \e ->
                                      runStateT (handler e) st
        quietThrowError e = StateT $ const $ quietThrowError e

instance MonadTracedErrors msg e m =>
    MonadTracedErrors msg e (ReaderT r m) where
        a `quietCatchError` handler = ReaderT $ \r -> runReaderT a r
                                      `quietCatchError` \e ->
                                      runReaderT (handler e) r
        quietThrowError e = ReaderT $ const $ quietThrowError e

instance (Monad m, Monoid w, MonadTracedErrors msg e m) =>
    MonadTracedErrors msg e (RWST r w s m) where
        a `quietCatchError` handler = RWST $ \r s -> runRWST a r s
                                      `quietCatchError` \e ->
                                      runRWST (handler e) r s
        quietThrowError = lift . quietThrowError

newtype TracedErrorT msg e m a = TracedErrorT { runTracedErrorT :: m a }
    deriving ( Functor
             , Monad
             , MonadIO
             , MonadReader r
             , MonadState st
             , MonadTrace msg
             , MonadWriter w
             )

instance (Functor (TracedErrorT msg e m), Monad m) =>
    Applicative (TracedErrorT msg e m) where
        pure = return
        (<*>) = ap

class EmbedsErrors msg e | msg -> e where
    embedThrow :: e -> msg
    embedCatch :: e -> msg
    embedCatchStart :: msg
    embedCatchEnd :: msg

class EmbedsMPlus msg where
    embedMzero :: msg
    embedMplusFailure :: msg

instance (MonadPlus m, EmbedsErrors msg e, EmbedsMPlus msg,  MonadTrace msg m) =>
    MonadPlus (TracedErrorT msg e m) where
        mzero = TracedErrorT $ logMessage embedMzero >> mzero
        a `mplus` b =
            let runA = runTracedErrorT a
                runB = do logMessage embedMplusFailure
                          runTracedErrorT b
            in TracedErrorT $ runA `mplus` runB

instance (MonadError e m, EmbedsErrors msg e, Error e, MonadTrace msg m) =>
    MonadError e (TracedErrorT msg e m) where
        throwError e = TracedErrorT $ do logMessage $ embedThrow e
                                         throwError e
        act `catchError` handler =
            TracedErrorT $ do
              val <- frameMessage embedCatchStart (runTracedErrorT act)
                     `catchError` \e ->
                     -- Frame the error catch so we can see what takes
                     -- place in the handler
                     frameMessage (embedCatch e) $ runTracedErrorT $ handler e
              logMessage embedCatchEnd
              return val

instance MonadError e m => MonadTracedErrors msg e (TracedErrorT msg e m) where
    a `quietCatchError` handler =
        TracedErrorT $ runTracedErrorT a `catchError` \e ->
            runTracedErrorT $ handler e
    quietThrowError = TracedErrorT . throwError

instance MonadTrans (TracedErrorT msg e) where
    lift = TracedErrorT

--------------------------------------------------
-- Ignoring tracing

-- |Transformer that puts a monad in MonadTrace by ignoring any tracing
newtype NoTraceT m a = NoTraceT { runNoTraceT :: m a }
    deriving ( Functor
             , Monad
             , MonadError e
             , MonadIO
             , MonadPlus
             , MonadReader r
             , MonadState st
             , MonadWriter w
             )

instance Monad m => MonadTrace () (NoTraceT m) where
    frameMessage = const id
    logMessage = const $ return ()
    untraced = id

instance MonadTrans NoTraceT where
    lift = NoTraceT

instance (Functor m, Monad m) => Applicative (NoTraceT m) where
    pure = return
    (<*>) = ap

instance MonadRandom m => MonadRandom (NoTraceT m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs

instance MonadTrace msg m => MonadTrace msg (ReaderT r m) where
    untraced act = ReaderT $ \r -> untraced $ runReaderT act r
    frameMessage msg act = ReaderT $ \r -> frameMessage msg $ runReaderT act r
    logMessage msg = lift $ logMessage msg

instance (Monoid w, MonadTrace msg m) => MonadTrace msg (RWST r w s m) where
    untraced act = RWST $ \r s -> untraced $ runRWST act r s
    frameMessage msg act = RWST $ \r s -> frameMessage msg $ runRWST act r s
    logMessage msg = lift $ logMessage msg

instance MonadTrace msg m => MonadTrace msg (StateT st m) where
    untraced act = StateT $ \s -> untraced $ runStateT act s
    frameMessage msg act = StateT $ \s -> frameMessage msg $ runStateT act s
    logMessage msg = lift $ logMessage msg

type Untraced a = NoTraceT Identity a

instance EmbedsErrors () e where
    embedCatch = const ()
    embedThrow = const ()
    embedCatchStart = ()
    embedCatchEnd = ()

instance EmbedsMPlus () where
    embedMzero = ()
    embedMplusFailure = ()

instance TraceString () where
    traceString = const ()

instance MonadError e m => MonadTracedErrors msg e (NoTraceT m) where
    a `quietCatchError` handler = NoTraceT $
                                  runNoTraceT a `catchError` \e ->
                                      runNoTraceT $ handler e
    quietThrowError = NoTraceT . throwError

-- |Ignore any tracing in an action
ignoreTracing :: Untraced a -> a
ignoreTracing = runIdentity . runNoTraceT
