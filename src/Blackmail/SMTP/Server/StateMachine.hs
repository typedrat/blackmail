module Blackmail.SMTP.Server.StateMachine (SMTP(..), SMTPStateId(..), SMTPStateData(..), StateType(..), SMTPEventId(..), SMTPEventData(..), EventType(..), SMTPHasSockAddr(..), SMTPHasClientName(..), SMTPHasSender(..), SMTPHasRecipients(..), SMTPHasRecipient(..), SMTPHasBody(..)) where

import Control.FSM.TH
import Control.FSM.Monad
import qualified Data.ByteString as BS
import Network.Socket (SockAddr)

import Blackmail.SMTP.Address

--

makeFSMTypes "SMTP" $ do
    let connectingA = attrib "sockAddr" [t| SockAddr |]
    connect <- event "Connection" connectingA
    connected <- state "Connected" connectingA
    transition initial connect connected

    let greetingA = attrib "clientName" [t| BS.ByteString |]
    helo <- event "HELO" greetingA
    ehlo <- event "EHLO" greetingA
    greeted <- state "Greeted" (connectingA >> greetingA)
    transition connected helo greeted
    transition connected ehlo greeted

    let senderA = attrib "sender" [t| Maybe Address |]
    mail <- event "MAIL" senderA
    hasSender <- state "HasSender" (connectingA >> greetingA >> senderA)
    transition greeted mail greeted -- When the sender is invalid
    transition greeted mail hasSender

    let rcptsA = attrib "recipients" [t| [Address] |]
    rcpt <- event "RCPT" $ attrib "recipient" [t| Address |]
    hasRecipients <- state "HasRecipients" (connectingA >> greetingA >> senderA >> rcptsA)
    transition hasSender rcpt hasSender -- When the recipient is invalid
    transition hasSender rcpt hasRecipients
    transition hasRecipients rcpt hasRecipients

    let bodyA = attrib "body" [t| BS.ByteString |]
    data_ <- event "DATA" (return ())
    waitingForBody <- state "WaitingForBody" (connectingA >> greetingA >> senderA >> rcptsA >> bodyA)
    transition hasRecipients data_ waitingForBody

    body <- event "DATALine" bodyA
    transition waitingForBody body waitingForBody

    received <- state "MailReceived" (connectingA >> greetingA)
    dataEnd <- event "DATAEnd" (return ())
    transition waitingForBody dataEnd received

    startTls <- event "STARTTLS" (return ())
    transition greeted startTls connected

    rset <- event "RSET" (return ())
    mapM_ (\x -> transition x rset greeted) [greeted, hasSender, hasRecipients, received]

    quit <- event "QUIT" (return ())
    final <- terminal "Disconnect" (return ())
    mapM_ (\x -> transition x quit final) [connected, greeted, hasSender, hasRecipients, received]

    vrfy <- event "VRFY" (return ())
    mapM_ (\x -> transition x vrfy x) [connected, greeted, hasSender, hasRecipients, received]

    noop <- event "NOOP" (return ())
    mapM_ (\x -> transition x noop x) [connected, greeted, hasSender, hasRecipients, received]

    unknown <- event "Unknown" (attrib "unknownCmd" [t| BS.ByteString |])
    mapM_ (\x -> transition x unknown x) [connected, greeted, hasSender, hasRecipients, received]

    invalid <- event "Invalid" (return ())
    mapM_ (\x -> transition x invalid x) [connected, greeted, hasSender, hasRecipients, received]
