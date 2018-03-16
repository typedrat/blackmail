{-# LANGUAGE DeriveDataTypeable #-}
module Blackmail.SMTP.Protocol (module Blackmail.SMTP.StateMachine, smtpConduit, smtpParser, smtpProtocol, smtpEncoder) where

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
import Blackmail.SMTP.Config
import Blackmail.SMTP.Monad
import Blackmail.SMTP.StateMachine
import Blackmail.SMTP.WireProtocol

--

hostname :: (MonadIO m, MonadReader r m, HasNetworkSettings r) => m BS.ByteString
hostname = view visibleHost >>= \case
    Just name -> return name
    Nothing -> BS.pack <$> liftIO getHostName

logFn :: (MonadIO m, MonadLogger m) => LogLevel -> SockAddr -> T.Text -> m ()
logFn lvl addr msg = do
    time <- liftIO getCurrentTime
    let addrT = showSockAddrT addr
        fmt = iso8601DateFormat (Just "%H:%M:%SZ")
        timestamp = T.pack $ formatTime defaultTimeLocale fmt time
        msg' = "[" <> timestamp <> ", " <> addrT <> "] " <> msg
    logWithoutLoc "" lvl msg'

smtpProtocol :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, MonadReader r m, MonadThrow m, HasSettings r IO) => Maybe ((AppData -> ConduitT (EventType SMTP) SMTPResponse m ()) -> ConduitT (EventType SMTP) SMTPResponse m ()) -> ConduitT (EventType SMTP) SMTPResponse m ()
smtpProtocol st = do
    awaitForever $ \event -> withMachineState $ \state -> case (state, event) of
        (Initial_ from, Connection_ via) -> do
            let addr = via ^. _sockAddr
            host <- hostname

            yield $ ServiceReady (host <> " ESMTP - blackmail, n. extortion or coercion by threats, especially of public exposure or criminal prosecution.")
            logFn LevelInfo addr "connected"

            doTransition from via $ ConnectedData addr

        (Connected_ from, HELO_ via) -> do
            let addr = from ^. _sockAddr
                client = via ^. _clientName

            host <- hostname
            yield $ MailActionCompleted (host <> " - Nice to meet you. I like romantic dinners and long Brownian walks on the beach.")
            logFn LevelInfo addr ("greeted with HELO as " <> T.decodeUtf8 client)

            doTransition from via $ GreetedData addr client

        (Connected_ from, EHLO_ via) -> do
            let addr = from ^. _sockAddr
                client = via ^. _clientName
                canStartTls = isJust st

            host <- hostname
            yield $ MailActionCompleted (host <> " - Nice to meet you. I like romantic dinners and long Brownian walks on the beach.")
            yield $ MailActionCompleted "PIPELINING"
            yield $ MailActionCompleted "8BITMIME"
            when canStartTls (yield $ MailActionCompleted "STARTTLS")
            logFn LevelInfo addr ("greeted with EHLO as " <> T.decodeUtf8 client)

            doTransition from via $ GreetedData addr client

        (Greeted_ from, MAIL_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = via ^. _sender
            allowed <- view allowSender
            isAllowed <- liftIO (allowed sender)

            if isAllowed
                then do
                    yield $ MailActionCompleted "ok!"
                    case sender of
                        Just sender -> logFn LevelDebug addr ("added sender <" <> T.pack (show sender) <> ">")
                        Nothing     -> logFn LevelDebug addr "added null sender <>"

                    doTransition from via $ HasSenderData addr client sender
                else do
                    yield $ ServiceNotAvailable "tough luck, kid"
                    case sender of
                        Just sender -> logFn LevelDebug addr ("denied sender <" <> T.pack (show sender) <> ">")
                        Nothing     -> logFn LevelDebug addr "denied null sender <>"

                    doTransition from via $ GreetedData addr client

        (HasSender_ from, RCPT_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipient = via ^. _recipient
            allowed <- view allowRecipient
            isAllowed <- liftIO (allowed recipient)

            if isAllowed
                then do
                    yield $ MailActionCompleted "ok!"
                    logFn LevelDebug addr ("added recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via $ HasRecipientsData addr client sender [recipient]
                else do
                    yield $ MailboxPermUnavailable "tough luck, kid"
                    logFn LevelDebug addr ("denied recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via $ HasSenderData addr client sender

        (HasRecipients_ from, RCPT_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipients = from ^. _recipients
                recipient = via ^. _recipient
            allowed <- view allowRecipient
            isAllowed <- liftIO (allowed recipient)

            if isAllowed
                then do
                    yield $ MailActionCompleted "ok!"
                    logFn LevelDebug addr ("added recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via $ HasRecipientsData addr client sender (recipient:recipients)
                else do
                    yield $ MailboxPermUnavailable "tough luck, kid"
                    logFn LevelDebug addr ("denied recipient <" <> T.pack (show recipient) <> ">")

                    doTransition from via $ HasRecipientsData addr client sender recipients

        (HasRecipients_ from, DATA_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipients = from ^. _recipients

            yield $ StartMailInput "Hit me with your best shot, fiiiiire away"
            logFn LevelDebug addr "got data command, waiting for body"

            doTransition from via $ WaitingForBodyData addr client sender recipients BS.empty

        (WaitingForBody_ from, DATALine_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipients = from ^. _recipients
                body = from ^. _body
                line = via ^. _body

            logFn LevelDebug addr "got line"

            doTransition from via $ WaitingForBodyData addr client sender recipients (body <> line <> "\r\n")

        (WaitingForBody_ from, DATAEnd_ via) -> do
            let addr = from ^. _sockAddr
                client = from ^. _clientName
                sender = from ^. _sender
                recipients = from ^. _recipients
                body = from ^. _body

            yield $ MailActionCompleted "everything's ready to go!"
            logFn LevelInfo addr "finished receiving message"

            send <- view sendMail
            liftIO $ send (Mail sender recipients body)

            doTransition from via $ MailReceivedData addr client

        (Greeted_ from, STARTTLS_ via) | isJust st -> do
            let addr = from ^. _sockAddr
                Just start = st

            start (smtpConduit Nothing)

    -- Trivial handlers:
        (_, VRFY_ via) ->
            yield $ CannotVRFY "Do or do not, there is no VRFY."

        (_, NOOP_ via) ->
            yield $ MailActionCompleted "Successfully did nothing."

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
            yield $ UnknownCommand "What does that mean?"
        (_, Invalid_ _) ->
            yield $ UnacceptableParams "What does that mean?"
        (_, _) ->
            yield $ BadCommandSequence "I don't understand what you're trying to do."


handleRSET :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, SMTPHasSockAddr (StateType' SMTP from) SockAddr, SMTPHasClientName (StateType' SMTP from) BS.ByteString, FSMValidTransition SMTP from RSET Greeted)
           => StateType' SMTP from -> EventType' SMTP RSET -> ConduitT i SMTPResponse m ()
handleRSET from via = do
    let addr = from ^. _sockAddr
        client = from ^. _clientName

    logFn LevelDebug addr "reset envelope"
    yield $ MailActionCompleted "The envelope will be lost in time, like tears in rain."

    doTransition from via $ GreetedData addr client

handleQUIT :: (MonadFSM SMTP m, MonadLogger m, MonadIO m, SMTPHasSockAddr (StateType' SMTP from) SockAddr, FSMValidTransition SMTP from QUIT Disconnect)
           => StateType' SMTP from -> EventType' SMTP QUIT -> ConduitT i SMTPResponse m ()
handleQUIT from via = do
    let addr = from ^. _sockAddr

    logFn LevelDebug addr "requested I close the socket"
    yield $ ServiceClosingChannel "Catch you on the flipside, dudemeister..."

    liftIO $ killThread =<< myThreadId

    doTransition from via $  DisconnectData

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

smtpConduit :: (MonadFSM SMTP m, MonadIO m, MonadLogger m, MonadThrow m, HasSettings r IO, MonadReader r m) => Maybe ((AppData -> ConduitT (EventType SMTP) SMTPResponse m ()) -> ConduitT (EventType SMTP) SMTPResponse m ()) -> AppData -> ConduitT i o m ()
smtpConduit st ad = smtpSource ad .| smtpProtocol st .| smtpSink ad
