module Blackmail.Server (runServer, module Blackmail.SMTP.Config) where

import Control.Lens.Operators
import Control.Monad.Logger
import Conduit
import Data.Conduit.Network
import UnliftIO

import Blackmail.SMTP.Config
import Blackmail.SMTP.Monad
import Blackmail.SMTP.Protocol

--

runServer :: (MonadUnliftIO m) => Settings m -> m ()
runServer settings = withRunInIO $ \run -> do
    let settings' = rebaseHandlers run settings
        serverSet = serverSettings (settings' ^. port) (settings' ^. host)
        filter _ lvl = lvl /= LevelDebug || settings' ^. showDebuggingLogs
        runFilteredLogger = runStderrLoggingT . filterLogger filter
    runTCPServer serverSet (runFilteredLogger . evalMachineT . runConduit . runReaderC settings' . smtpConduit)
