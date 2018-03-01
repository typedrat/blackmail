{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Blackmail.SMTP.Protocol.Types (SMTPStateId(..), SMTPStateData(..), TypeInState, UpdateState(..), SMTPState(..)) where

import qualified Data.ByteString.Char8 as BS
import Data.Kind
import GHC.TypeLits
import Network.Socket (SockAddr)

import Blackmail.SMTP.Protocol.TH

--

-- | This is never used as a type, but only serves as an algebraic data kind to flag the nodes of the protocol FSM.
data SMTPStateId = Connected -- ^ The initial state that occurs when a client first connects.
                 | Greeted -- ^ The client has already greeted the server with @HELO@ or @EHLO@.
                 | Disconnected -- ^ The client has either @QUIT@ or just closed the socket.
                 deriving (Eq, Ord)

mkCmpTF ''SMTPStateId

data SMTPStateData (a :: SMTPStateId) = SMTPStateData
                     { _stSockAddr :: TypeInState a Connected SockAddr
                     , _stHostName :: TypeInState a Greeted BS.ByteString
                     }

deriving instance ( Show (TypeInState a Connected SockAddr)
                  , Show (TypeInState a Greeted BS.ByteString)
                  ) => Show (SMTPStateData a)

type TypeInState (cur :: SMTPStateId) (avail :: SMTPStateId) (val :: Type) = TypeInState' cur avail val
-- ^ If @a >= b@, @TypeInState a b v ~ v@. Otherwise, @TypeInState a b v ~ ()@.

-- | Allows a way to write functions that look natural but generically update the state along any /correct/ transition.
class UpdateState (a :: SMTPStateId) (b :: SMTPStateId) where
    type StateChanges a b :: [Type]
    -- ^ A type-level list of the types of the fields that are being added.
    updateState :: SMTPStateData a -> UpdateFunction (StateChanges a b) b
    -- ^ This function will magically have the type of a function that takes a @'SMTPStateData' a@,
    --   then each of the new fields, and then gives a new @'SMTPStateData' b@.
    --
    --   For example, if going from 'Connected' to 'Greeted' adds a 'BS.ByteString':
    --
    --   > updateState :: SMTPStateData Connected -> BS.ByteString -> SMTPStateData Greeted

-- * Type hacking

type family TypeInState'' (ord :: Ordering) (val :: Type) :: Type where
    TypeInState'' LT t = ()
    TypeInState'' EQ t = t
    TypeInState'' GT t = t

type TypeInState' (cur :: SMTPStateId) (avail :: SMTPStateId) (val :: Type) = TypeInState'' (CmpSMTPStateId cur avail) val

type family UpdateFunction (changes :: [Type]) (res :: SMTPStateId) = out | out -> changes res where
    UpdateFunction (c:cs)  res = c -> UpdateFunction cs res
    UpdateFunction '[] res = SMTPStateData res
-- This has a slightly weird type because of the restrictions on what type families GHC can prove to be injective.

data SMTPState = Connected_ (SMTPStateData Connected)
               | Greeted_ (SMTPStateData Greeted)
               | Disconnected_ (SMTPStateData Disconnected)
               deriving (Show)
-- ^ === An excerpt from /Mankind Has Yet To Recognize My Genius, vol. 14/ by Alexis Williams:
--
-- There's no library that implements streaming as an /indexed/ monad transformer.
--
-- Therefore, in order to protect the state, I have to use an unindexed state monad that holds a wrapper type that then is simply a coproduct
-- of each possible index value and the /actual/ state type, and then must write my actions /not/ as simple monadic actions that understand the
-- stateful structure that they are executing in, but as functions in another, more generic monad that take unwrapped input states as arguments
-- and return the output state to be wrapped up again before getting 'Control.Monad.State.Class.put' back into the 'Control.Monad.State.Lazy.StateT'.

mkUpdateStateInst 'Connected 'Greeted
