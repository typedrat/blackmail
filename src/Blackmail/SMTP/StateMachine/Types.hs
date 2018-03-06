module Blackmail.SMTP.StateMachine.Types (SMTP(..), SMTPStateId(..), SMTPStateData(..), SMTPEventId(..), SMTPEventData(..), EventType(..), SMTPHasClientName(..), SMTPHasSender(..), SMTPHasRecipients(..), SMTPHasRecipient(..), SMTPHasBody(..), makeSMTPFSMInstance) where

import Control.FSM.TH
import Control.FSM.Monad
import qualified Data.ByteString as BS

import Blackmail.SMTP.Address

--

makeFSMTypes "SMTP" $ do
    let greetingA = attrib "clientName" [t| BS.ByteString |]
    helo <- event "HELO" greetingA
    ehlo <- event "EHLO" greetingA
    greeted <- state "Greeted" greetingA
    transition initial helo greeted
    transition initial ehlo greeted

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
    mapM_ (\x -> transition x quit final) [initial, greeted, hasSender, hasRecipients]

    vrfy <- event "VRFY" (return ())
    mapM_ (\x -> transition x vrfy x) [initial, greeted, hasSender, hasRecipients]

    noop <- event "NOOP" (return ())
    mapM_ (\x -> transition x noop x) [initial, greeted, hasSender, hasRecipients]

instance EffectfulFSM SMTP where
    type EffectMonad SMTP = IO
