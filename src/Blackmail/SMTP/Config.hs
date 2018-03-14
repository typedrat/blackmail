module Blackmail.SMTP.Config (NetworkSettings(), Handlers(), rebaseHandlers, Settings(), Mail(..), HasNetworkSettings(..), HasHandlers(..), HasSettings(..), defaultNetworkSettings, defaultHandlers, defaultSettings) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network

import Blackmail.SMTP.Address

data NetworkSettings = NetworkSettings
                     { _host :: HostPreference
                     , _port :: Int
                     , _visibleHost :: Maybe (BS.ByteString)
                     }

defaultNetworkSettings = NetworkSettings "*6" 8025 Nothing

data Handlers m = Handlers
              { _allowSender :: Maybe (Address) -> m Bool
              , _allowRecipient :: Address -> m Bool
              , _sendMail :: Mail -> m ()
              }

defaultHandlers :: (MonadIO m) => Handlers m
defaultHandlers = Handlers (\_ -> return True) (\_ -> return True) (liftIO . print)

rebaseHandlers :: (Monad m, Monad n) => (forall a. m a -> n a) -> Settings m -> Settings n
rebaseHandlers f old@(Settings _ (Handlers aS aR sM) _) = old { _sHandlers = Handlers (f . aS) (f . aR) (f . sM) }


data Settings m = Settings
              { _sNetworkSettings :: NetworkSettings
              , _sHandlers :: Handlers m
              , _sShowDebuggingLogs :: Bool
              }

defaultSettings :: (MonadIO m) => Settings m
defaultSettings = Settings defaultNetworkSettings defaultHandlers True

data Mail = Mail
          { from :: Maybe Address
          , to :: [Address]
          , body :: BS.ByteString
          }
          deriving (Eq)

instance Show Mail where
    show (Mail from to body) = unlines $ ["From: <" ++ from' ++ ">"] ++ fmap (\t -> "To: <" ++ show t ++ ">") to ++ [BS.unpack body]
        where from' = maybe "" show from

makeLensesWith classyRules ''NetworkSettings
makeLensesWith classyRules ''Handlers
makeLenses ''Settings

class (HasHandlers c m, HasNetworkSettings c) => HasSettings c m where
    settings :: Lens' c (Settings m)

    showDebuggingLogs :: Lens' c Bool
    showDebuggingLogs = settings . sShowDebuggingLogs

instance HasNetworkSettings (Settings m) where
    networkSettings = sNetworkSettings

instance HasHandlers (Settings m) m where
    handlers = sHandlers

instance HasSettings (Settings m) m where
    settings = id
