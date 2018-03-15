module Blackmail.Server (runServer, module Blackmail.SMTP.Config) where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Logger
import Conduit
import Data.Conduit.Network
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
    let settings' = rebaseHandlers run settings
        serverSets = (\port -> serverSettings port (settings' ^. host)) <$> (settings' ^. ports)
        filter _ lvl = lvl /= LevelDebug || settings' ^. showDebuggingLogs
        runFilteredLogger = runStderrLoggingT . filterLogger filter
    ids <- forM serverSets $ \set -> do
        liftIO $ putStrLn ("Listening on port " ++ show (getPort set))
        async $ runTCPServer set (runFilteredLogger . runResourceT . evalMachineT . runConduit . runReaderC settings' . (\ad -> do
                logFn <- mkLogFn
                bracketP (pure ad) (\ad -> logFn LevelInfo (appSockAddr ad) "disconnected") smtpConduit
            ))

    mapM wait ids
    return ()
