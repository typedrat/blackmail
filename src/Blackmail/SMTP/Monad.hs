{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Blackmail.SMTP.Monad () where

import Control.Monad.Logger

import Blackmail.SMTP.Protocol.Types
import Blackmail.SMTP.WireProtocol


