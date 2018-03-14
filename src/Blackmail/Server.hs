module Blackmail.Server (runServer, module Blackmail.SMTP.Config) where

import Control.Lens.Operators
import Control.Monad
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
        serverSets = (\port -> serverSettings port (settings' ^. host)) <$> (settings' ^. ports)
        filter _ lvl = lvl /= LevelDebug || settings' ^. showDebuggingLogs
        runFilteredLogger = runStderrLoggingT . filterLogger filter
    ids <- forM serverSets $ \set -> do
        liftIO $ putStrLn ("Listening on port " ++ show (getPort set))
        async $ runTCPServer set (runFilteredLogger . evalMachineT . runConduit . runReaderC settings' . smtpConduit)

    mapM wait ids
    return ()
