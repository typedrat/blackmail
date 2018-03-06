module Blackmail.SMTP.StateMachine (module Blackmail.SMTP.StateMachine.Types) where

import Control.FSM.Monad
import Control.Lens
import qualified Data.ByteString as BS

import Blackmail.SMTP.Address
import Blackmail.SMTP.StateMachine.Types

--

instance (FSM SMTP, FSMValidTransition SMTP Initial via Greeted, SMTPHasClientName (SMTPEventData via) BS.ByteString) => FSMTransition SMTP Initial via Greeted where
    doTransition from via = return (GreetedData $ via ^. _clientName)

instance (FSM SMTP) => FSMTransition SMTP Greeted MAIL HasSender where
    doTransition from via = return (HasSenderData (from ^. _clientName) (via ^. _sender))

instance (FSM SMTP) => FSMTransition SMTP HasSender RCPT HasRecipients where
    doTransition from via = return (HasRecipientsData (from ^. _clientName) (from ^. _sender) [via ^. _recipient])

instance (FSM SMTP) => FSMTransition SMTP HasRecipients RCPT HasRecipients where
    doTransition from via = return (from & _recipients %~ (via ^. _recipient :))

instance (FSM SMTP) => FSMTransition SMTP HasRecipients DATA WaitingForBody where
    doTransition from _ = return (WaitingForBodyData (from ^. _clientName) (from ^. _sender) (from ^. _recipients))

instance (FSM SMTP) => FSMTransition SMTP WaitingForBody DATABody Greeted where
    doTransition from _ = return (GreetedData (from ^. _clientName))

instance (FSM SMTP, FSMValidTransition SMTP a NOOP a) => FSMTransition SMTP a NOOP a where
    doTransition from _ = return from

instance (FSM SMTP, FSMValidTransition SMTP a VRFY a) => FSMTransition SMTP a VRFY a where
    doTransition from _ = return from

instance (FSM SMTP, FSMValidTransition SMTP a RSET Greeted, SMTPHasClientName (SMTPStateData a) BS.ByteString) => FSMTransition SMTP a RSET Greeted where
    doTransition from _ = return (GreetedData $ from ^. _clientName)

instance (FSM SMTP, FSMValidTransition SMTP a QUIT Disconnect) => FSMTransition SMTP a QUIT Disconnect where
    doTransition _ _ = return DisconnectData

makeSMTPFSMInstance
