module Blackmail.SMTP.Monad (module Control.FSM.Monad) where

import Control.FSM.Monad
import Control.FSM.Monad.Internal
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Conduit

import Blackmail.SMTP.StateMachine

instance (MonadFSM mach m) => MonadFSM mach (ConduitT i o m) where
    getMachineState = lift getMachineState
    putMachineState = lift . putMachineState

instance MonadLogger m => MonadLogger (MachineT mach m)

instance MonadThrow m => MonadThrow (MachineT mach m) where
    throwM = lift . throwM
