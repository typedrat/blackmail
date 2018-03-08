{-# LANGUAGE LambdaCase #-}
module Blackmail.SMTP.StateMachine (module Blackmail.SMTP.StateMachine.Types) where

import Control.FSM.Monad
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import qualified Data.ByteString as BS
import Text.Megaparsec

import Blackmail.SMTP.Address
import Blackmail.SMTP.StateMachine.Types
import Blackmail.SMTP.WireProtocol

--

instance (MonadFSM mach m) => MonadFSM mach (ParsecT e BS.ByteString m)

smtpP :: (MonadFSM SMTP m, Ord e) => ParsecT e BS.ByteString m (EventType SMTP)
smtpP = withMachineState $ \case
    WaitingForBody_ _ -> dataBodyP
    _                 -> commandP
