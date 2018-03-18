{-# LANGUAGE TypeApplications #-}
module Blackmail.SMTP.Server.Protocol (module Blackmail.SMTP.Server.StateMachine, smtpConduit, smtpParser, smtpProtocol, smtpEncoder) where

import Control.Concurrent (killThread, myThreadId)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import {- Data. -} Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Data.Maybe
import Data.Semigroup
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HostName
import Network.Socket.SockAddr

import Blackmail.SMTP.Address
import Blackmail.SMTP.Server.Config
import Blackmail.SMTP.Server.Monad
import Blackmail.SMTP.Server.StateMachine
import Blackmail.SMTP.Server.WireProtocol

--

hostname :: (MonadIO m, MonadReader r m, HasNetworkSettings r) => m BS.ByteString
hostname = view visibleHost >>= \case
    Just name -> return name
    Nothing -> BS.pack <$> liftIO getHostName

instance SMTPHasSockAddr SockAddr SockAddr where
    _sockAddr = id

logFn :: (MonadIO m, MonadLogger m, SMTPHasSockAddr from SockAddr) => LogLevel -> from -> T.Text -> m ()
logFn lvl from msg = do
    time <- liftIO getCurrentTime
    let addrT = showSockAddrT (from ^. _sockAddr)
        fmt = iso8601DateFormat (Just "%H:%M:%SZ")
        timestamp = T.pack $ formatTime defaultTimeLocale fmt time
        msg' = "[" <> timestamp <> ", " <> addrT <> "] " <> msg
    logWithoutLoc "" lvl msg'

smtpProtocol :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, MonadReader r m, MonadThrow m, HasSettings r IO) => Maybe (IO ()) -> ConduitT (EventType SMTP) SMTPResponse m ()
smtpProtocol st = do
    awaitForever $ \event -> withMachineState $ \state -> case (state, event) of
        (Initial_ from, Connection_ via) -> do
            let addr = via ^. _sockAddr
            host <- hostname

            yield $ ServiceReady [host <> " ESMTP - blackmail, n. extortion or coercion by threats, especially of public exposure or criminal prosecution."]
            logFn LevelInfo addr "connected"

            doTransition from via (ConnectedData addr)

        (Connected_ from, HELO_ via) -> do
            let client = via ^. _clientName

            host <- hostname
            yield $ MailActionCompleted [host <> " - Nice to meet you. I like romantic dinners and long Brownian walks on the beach."]
            logFn LevelInfo from ("greeted with HELO as " <> T.decodeUtf8 client)

            doTransition from via (from & _transition .~ client :: StateType' SMTP Greeted)

        (Connected_ from, EHLO_ via) -> do
            let client = via ^. _clientName
                canStartTls = isJust st

            host <- hostname
            -- This is a hideous way to do this, but the multi-line response has to be one single packet
            let banner = host <> " - Nice to meet you. I like romantic dinners and long Brownian walks on the beach."
                exts = [banner, "PIPELINING", "8BITMIME", "SMTPUTF8", "SIZE 31457280"]
                msg = MailActionCompleted $ if canStartTls then exts ++ ["250-STARTTLS"] else exts

            yield msg
            logFn LevelInfo from ("greeted with EHLO as " <> T.decodeUtf8 client)

            doTransition from via (from & _transition .~ client :: StateType' SMTP Greeted)

        (Greeted_ from, MAIL_ via) -> do
            let sender = via ^. _sender

            isAllowed <- view allowSender >>= \f -> liftIO (f sender)

            if isAllowed
                then do
                    yield $ MailActionCompleted ["ok!"]
                    case sender of
                        Just sender -> logFn LevelDebug from ("added sender <" <> T.pack (show sender) <> ">")
                        Nothing     -> logFn LevelDebug from "added null sender <>"

                    doTransition from via (from & _transition .~ sender :: SMTPStateData HasSender)
                else do
                    yield $ ServiceNotAvailable ["tough luck, kid"]
                    case sender of
                        Just sender -> logFn LevelDebug from ("denied sender <" <> T.pack (show sender) <> ">")
                        Nothing     -> logFn LevelDebug from "denied null sender <>"

                    doTransition from via from

        (HasSender_ from, RCPT_ via) -> do
            let recipient = via ^. _recipient

            isAllowed <- view allowRecipient >>= \f -> liftIO (f recipient)

            if isAllowed
                then do
                    yield $ MailActionCompleted ["ok!"]
                    logFn LevelDebug from ("added recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via (from & _transition .~ [recipient] :: SMTPStateData HasRecipients)
                else do
                    yield $ MailboxPermUnavailable ["tough luck, kid"]
                    logFn LevelDebug from ("denied recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via from

        (HasRecipients_ from, RCPT_ via) -> do
            let recipient = via ^. _recipient
            allowed <- view allowRecipient
            isAllowed <- liftIO (allowed recipient)

            if isAllowed
                then do
                    yield $ MailActionCompleted ["ok!"]
                    logFn LevelDebug from ("added recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via (from & _recipients %~ (recipient:))
                else do
                    yield $ MailboxPermUnavailable ["tough luck, kid"]
                    logFn LevelDebug from ("denied recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via from

        (HasRecipients_ from, DATA_ via) -> do
            yield $ StartMailInput ["Hit me with your best shot, fiiiiire away"]
            logFn LevelDebug from "got data command, waiting for body"

            doTransition from via (from & _transition .~ "" :: SMTPStateData WaitingForBody)

        (WaitingForBody_ from, DATALine_ via) -> do
            let line = via ^. _body

            logFn LevelDebug from "got line"

            doTransition from via $ from & _body %~ flip BS.append (line <> "\r\n")

        (WaitingForBody_ from, DATAEnd_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipients = from ^. _recipients
                body = from ^. _body

            yield $ MailActionCompleted ["everything's ready to go!"]
            logFn LevelInfo from "finished receiving message"

            send <- view sendMail
            liftIO $ send (Mail sender recipients body)

            doTransition from via (from & _transition .~ () :: SMTPStateData MailReceived)

        (Greeted_ from, STARTTLS_ via) | isJust st -> do
            let addr = from ^. _sockAddr
                Just start = st
            logFn LevelInfo from "switched to using TLS"
            yield $ ServiceReady ["do it ya nerd"]
            liftIO start

    -- Trivial handlers:
        (_, VRFY_ via) ->
            yield $ CannotVRFY ["Do or do not, there is no VRFY."]

        (_, NOOP_ via) ->
            yield $ MailActionCompleted ["Successfully did nothing."]

        (Greeted_ from, RSET_ via) -> handleRSET from via
        (HasSender_ from, RSET_ via) -> handleRSET from via
        (HasRecipients_ from, RSET_ via) -> handleRSET from via
        (MailReceived_ from, RSET_ via) -> handleRSET from via

        (Connected_ from, QUIT_ via) -> handleQUIT from via
        (Greeted_ from, QUIT_ via) -> handleQUIT from via
        (HasSender_ from, QUIT_ via) -> handleQUIT from via
        (HasRecipients_ from, QUIT_ via) -> handleQUIT from via
        (MailReceived_ from, QUIT_ via) -> handleQUIT from via

        (_, Unknown_ _) ->
            yield $ UnknownCommand ["What does that mean?"]
        (_, Invalid_ _) ->
            yield $ UnacceptableParams ["What does that mean?"]
        (_, _) ->
            yield $ BadCommandSequence ["I don't understand what you're trying to do."]


handleRSET :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, SMTPHasSockAddr (StateType' SMTP from) SockAddr, SMTPHasClientName (StateType' SMTP from) BS.ByteString, FSMValidTransition SMTP from RSET Greeted)
           => StateType' SMTP from -> EventType' SMTP RSET -> ConduitT i SMTPResponse m ()
handleRSET from via = do
    let addr = from ^. _sockAddr
        client = from ^. _clientName

    logFn LevelDebug from "reset envelope"
    yield $ MailActionCompleted ["The envelope will be lost in time, like tears in rain."]

    doTransition from via $ GreetedData addr client

handleQUIT :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, SMTPHasSockAddr (StateType' SMTP from) SockAddr, FSMValidTransition SMTP from QUIT Disconnect)
           => StateType' SMTP from -> EventType' SMTP QUIT -> ConduitT i SMTPResponse m ()
handleQUIT from via = do
    let addr = from ^. _sockAddr

    logFn LevelDebug from "requested I close the socket"
    yield $ ServiceClosingChannel ["Catch you on the flipside, dudemeister..."]

    liftIO $ killThread =<< myThreadId

    doTransition from via $ DisconnectData

--

smtpParser :: (MonadFSM SMTP m, MonadThrow m) => ConduitT BS.ByteString (EventType SMTP) m ()
smtpParser = conduitParser msgP .| mapC snd

smtpSource :: (MonadFSM SMTP m, MonadThrow m, MonadIO m) => AppData -> ConduitT i (EventType SMTP) m ()
smtpSource ad = do
    yield . Connection_ . ConnectionData $ appSockAddr ad
    appSource ad .| smtpParser

smtpEncoder :: (Monad m) => ConduitT SMTPResponse BS.ByteString m ()
smtpEncoder = mapC responseToBS

smtpSink :: (MonadIO m) => AppData -> ConduitT SMTPResponse o m ()
smtpSink ad = smtpEncoder .| appSink ad

smtpConduit :: (MonadFSM SMTP m, MonadIO m, MonadLogger m, MonadThrow m, HasSettings r IO, MonadReader r m) => Maybe (IO ()) -> AppData -> ConduitT i o m ()
smtpConduit st ad = smtpSource ad .| smtpProtocol st .| smtpSink ad
