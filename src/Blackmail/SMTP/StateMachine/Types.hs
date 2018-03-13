module Blackmail.SMTP.StateMachine.Types (SMTP(..), SMTPStateId(..), SMTPStateData(..), StateType(..), SMTPEventId(..), SMTPEventData(..), EventType(..), SMTPHasSockAddr(..), SMTPHasClientName(..), SMTPHasSender(..), SMTPHasRecipients(..), SMTPHasRecipient(..), SMTPHasBody(..)) where

import Control.FSM.TH
import Control.FSM.Monad
import qualified Data.ByteString as BS
import Network.Socket (SockAddr)

import Blackmail.SMTP.Address

--

makeIllustratedFSMTypes "SMTP" $ do
    let connectingA = attrib "sockAddr" [t| String |]
    connect <- event "Connection" connectingA
    connected <- state "Connected" connectingA
    transition initial connect connected

    let greetingA = attrib "clientName" [t| BS.ByteString |]
    helo <- event "HELO" greetingA
    ehlo <- event "EHLO" greetingA
    greeted <- state "Greeted" greetingA
    transition connected helo greeted
    transition connected ehlo greeted

    let senderA = attrib "sender" [t| Maybe Address |]
    mail <- event "MAIL" senderA
    hasSender <- state "HasSender" (greetingA >> senderA)
    transition greeted mail hasSender

    let rcptsA = attrib "recipients" [t| [Address] |]
    rcpt <- event "RCPT" $ attrib "recipient" [t| Address |]
    hasRecipients <- state "HasRecipients" (greetingA >> senderA >> rcptsA)
    transition hasSender rcpt hasRecipients
    transition hasRecipients rcpt hasRecipients

    data_ <- event "DATA" (return ())
    waitingForBody <- state "WaitingForBody" (greetingA >> senderA >> rcptsA)
    transition hasRecipients data_ waitingForBody

    let bodyA = attrib "body" [t| BS.ByteString |]
    body <- event "DATABody" bodyA
    transition waitingForBody body greeted

    rset <- event "RSET" (return ())
    mapM_ (\x -> transition x rset greeted) [greeted, hasSender, hasRecipients]

    quit <- event "QUIT" (return ())
    final <- terminal "Disconnect" (return ())
    mapM_ (\x -> transition x quit final) [connected, greeted, hasSender, hasRecipients]

    vrfy <- event "VRFY" (return ())
    mapM_ (\x -> transition x vrfy x) [connected, greeted, hasSender, hasRecipients]

    noop <- event "NOOP" (return ())
    mapM_ (\x -> transition x noop x) [connected, greeted, hasSender, hasRecipients]

    unknown <- event "Unknown" (attrib "unknownCmd" [t| BS.ByteString |])
    mapM_ (\x -> transition x unknown x) [connected, greeted, hasSender, hasRecipients]

    invalid <- event "Invalid" (return ())
    mapM_ (\x -> transition x invalid x) [connected, greeted, hasSender, hasRecipients]
