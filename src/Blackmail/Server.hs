module Blackmail.Server (runServer, module Blackmail.SMTP.Config) where

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Conduit
import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Network.Socket.SockAddr
import UnliftIO

import Blackmail.SMTP.Config
import Blackmail.SMTP.Monad
import Blackmail.SMTP.Protocol

--

mkLogFn :: (MonadLoggerIO m) => m (LogLevel -> SockAddr -> T.Text -> IO ())
mkLogFn = do
    f <- askLoggerIO

    return $ \lvl addr msg -> do
        time <- liftIO getCurrentTime
        let addrT = showSockAddrT addr
            fmt = iso8601DateFormat (Just "%H:%M:%SZ")
            timestamp = T.pack $ formatTime defaultTimeLocale fmt time
            msg' = "[" <> timestamp <> ", " <> addrT <> "] " <> msg

        f defaultLoc "" lvl (toLogStr msg')

runServer :: (MonadUnliftIO m) => Settings m -> m ()
runServer settings = withRunInIO $ \run -> do
    let host' = settings ^. host
        filter _ lvl = lvl /= LevelDebug || settings ^. showDebuggingLogs
        runFilteredLogger = runStderrLoggingT . filterLogger filter

    let cert = settings ^. tlsCert
        key  = settings ^. tlsKey
        hasTls = isJust cert && isJust key && (settings ^. enableTls)

        settings' = rebaseHandlers run settings & enableTls .~ hasTls

        runTCPServer' :: Int -> IO ()
        runTCPServer' port
            | hasTls = do
                let Just cert' = cert
                    Just key' = key
                    config = tlsConfig host' port cert' key'
                    f st = st (mkServer $ smtpConduit Nothing)
                runTCPServerStartTLS config (\(ad, st) -> mkServer (smtpConduit $ Just (f st)) ad)
            | otherwise = runTCPServer (serverSettings port host') $ mkServer (smtpConduit Nothing)

        mkServer app ad = runFilteredLogger . runResourceT . evalMachineT . runConduit . runReaderC settings' $ do
            logFn <- mkLogFn
            bracketP (return ad) (\ad -> logFn LevelInfo (appSockAddr ad) "disconnected") app

    ids <- liftIO . forM (settings ^. ports) $ \port -> do
        putStrLn ("Listening on port " ++ show port)
        async $ runTCPServer' port

    mapM wait ids
    return ()
