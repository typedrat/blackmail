module Control.FSM.Monad.Internal (MachineT(..), runMachineT, evalMachineT, EventIdKind, StateIdKind, FSM(..), FSMValidTransition(..), FSMTransitionable(..), MonadFSM(..)) where

import Control.Applicative
import Control.Lens.Setter
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.State.Lazy as SL
import Data.Kind
import GHC.TypeLits

-- | This monad transformer wraps the effect monad @m@ that the FSM @mach@ uses. For 'mtl' style code, you'll have to derive any other instances you need.
newtype MachineT mach m a = MachineT { unMachineT :: SS.StateT (StateType mach) m a }
                          deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

-- | This converts the 'MachineT' representation of a state machine's actions into a monadic action in the effect monad.
runMachineT :: (FSM mach) => MachineT mach m a -> m (a, StateType mach)
runMachineT m = SS.runStateT (unMachineT m) initialState

evalMachineT :: (FSM mach, Monad m) => MachineT mach m a -> m a
evalMachineT = fmap fst . runMachineT

instance (MonadError e m) => MonadError e (MachineT mach m) where
    throwError = lift . throwError
    catchError (MachineT m) handler = MachineT $ catchError m (unMachineT . handler)

instance (MonadReader r m) => MonadReader r (MachineT mach m) where
    ask = lift ask
    reader = lift . reader
    local f (MachineT m) = MachineT $ local f m

instance (MonadWriter w m) => MonadWriter w (MachineT mach m) where
    writer = lift . writer
    tell = lift . tell
    listen (MachineT m) = MachineT $ listen m
    pass (MachineT m) = MachineT $ pass m

instance (MonadState s m) => MonadState s (MachineT mach m) where
    state f = MachineT $ lift (state f)

instance (MonadIO m) => MonadIO (MachineT mach m) where
    liftIO = lift . liftIO

instance (MonadFSM mach m)           => MonadFSM mach (ExceptT   e m)
instance (MonadFSM mach m)           => MonadFSM mach (ReaderT   r m)
instance (MonadFSM mach m, Monoid w) => MonadFSM mach (WriterT   w m)
instance (MonadFSM mach m)           => MonadFSM mach (SS.StateT s m)
instance (MonadFSM mach m)           => MonadFSM mach (SL.StateT s m)

type family EventIdKind event :: Type
type family StateIdKind event :: Type

-- | An instance of this class represents a particular FSM generated by this library.
class FSM mach where
    -- | This holds @mach@'s event data type, obfuscating the event type.
    data EventType mach :: Type
    -- | This is the base event data type.
    type EventType' mach = (out :: EventIdKind mach -> Type) | out -> mach
    -- | This holds @mach@'s state data type, obfuscating the event type.
    data StateType mach :: Type
    -- | This is the base state data type.
    type StateType' mach = (out :: StateIdKind mach -> Type) | out -> mach

    wrapEvent :: EventType' mach a -> EventType mach
    wrapState :: StateType' mach a -> StateType mach

    initialState :: StateType mach

class FSMValidTransition mach (from :: state) (via :: event) (to :: state)

class FSMTransitionable from to where
    type FSMTransitionDifference from to
    _transition :: Setter from to () (FSMTransitionDifference from to)

class (FSM mach, Monad m) => MonadFSM mach m | m -> mach where
    getMachineState :: m (StateType mach)
    default getMachineState :: (MonadFSM mach n, MonadTrans t, m ~ t n) => m (StateType mach)
    getMachineState = lift getMachineState

    putMachineState :: StateType mach -> m ()
    default putMachineState :: (MonadFSM mach n, MonadTrans t, m ~ t n) => StateType mach -> m ()
    putMachineState = lift . putMachineState

    withMachineState :: (StateType mach -> m a) -> m a
    withMachineState = (getMachineState >>=)

    doTransition :: (FSMValidTransition mach from via to) => StateType' mach from -> EventType' mach via -> StateType' mach to -> m ()
    doTransition _ _ action = putMachineState (wrapState action)

instance (FSM mach, Monad m) => MonadFSM mach (MachineT mach m) where
    getMachineState = MachineT get
    putMachineState = MachineT . put
