module Blackmail.SMTP.Config (NetworkSettings(), Handlers(), rebaseHandlers, Settings(), Mail(..), HasNetworkSettings(..), HasHandlers(..), HasSettings(..), defaultHandlers, defaultSettings) where

import Control.Lens hiding ((.=))
import Data.Conduit.Network
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.String
import qualified Data.Text.Encoding as T
import Data.Yaml

import Blackmail.SMTP.Address

data NetworkSettings = NetworkSettings
                     { _host :: HostPreference
                     , _ports :: [Int]
                     , _visibleHost :: Maybe (BS.ByteString)
                     , _tlsCert :: Maybe FilePath
                     , _tlsKey :: Maybe FilePath
                     }

instance FromJSON NetworkSettings where
    parseJSON = withObject "network setttings" $ \o -> do
        _host <- fromString <$> (o .:? "host" .!= "*6")
        _ports <- o .:? "ports" .!= [8025]
        _visibleHost <- fmap T.encodeUtf8 <$> o .:? "visible-host"
        _tlsCert <- o .:? "tls-cert-path"
        _tlsKey <- o .:? "tls-key-path"
        return NetworkSettings{..}


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

instance (MonadIO m) => FromJSON (Settings m) where
    parseJSON = withObject "settings" $ \o -> do
        _sNetworkSettings <- o .: "network"
        _sHandlers <- pure defaultHandlers
        _sShowDebuggingLogs <- o .:? "show-debug-logs" .!= True
        return Settings{..}

defaultSettings :: [Value]
defaultSettings = [object ["network" .= object []]]

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
