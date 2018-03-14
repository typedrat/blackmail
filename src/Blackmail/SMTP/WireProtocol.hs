module Blackmail.SMTP.WireProtocol (msgP, SMTPResponse(..), responseToBS) where

import Control.Applicative
import Control.FSM.Monad
import Data.Attoparsec.ByteString.Char8 as P hiding (skipSpace)
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.Semigroup

import Blackmail.SMTP.Address
import Blackmail.SMTP.StateMachine

--

msgP :: Parser (EventType SMTP)
msgP = heloP <|> ehloP <|> mailP <|> rcptP <|> dataP <|> rsetP <|> noopP <|> quitP <|> vrfyP <|> dataEndP <|> dataLineP
    where
        cmdP :: BS.ByteString -> Parser (EventType SMTP) -> Parser (EventType SMTP)
        cmdP cmd p = skipSpace *> stringCI cmd *> ((p <* space `manyTill` endOfLine) <|> (Invalid_ InvalidData <$ anyChar `manyTill` endOfLine))

        skipSpace :: Parser ()
        skipSpace = skipWhile (\c -> c == ' ' || c == '\t')

        clientName :: Parser BS.ByteString
        clientName = P.takeWhile1 (not . isSpace)

        arg :: Parser a -> Parser a
        arg p = skipSpace *> p <* skipSpace

        argL :: BS.ByteString -> Parser BS.ByteString
        argL s = arg $ stringCI s

        heloP, ehloP, mailP, rcptP, dataP, rsetP, noopP, quitP, vrfyP, dataEndP :: Parser (EventType SMTP)
        heloP = cmdP "HELO" $ HELO_ . HELOData <$> arg clientName
        ehloP = cmdP "EHLO" $ EHLO_ . EHLOData <$> arg clientName
        mailP = cmdP "MAIL" $ argL "FROM:"
                           *> arg (fmap (MAIL_ . MAILData . Just) addressP <|> MAIL_ (MAILData Nothing) <$ string "<>")
                           -- I'm not going to do Q-P conversion, but I have to pretend I care
                           <* optional (stringCI "BODY=7BIT" <|> stringCI "BODY=8BITMIME")
        rcptP = cmdP "RCPT" $ argL "TO:"
                           *> arg (RCPT_ . RCPTData <$> addressP)
        dataP = cmdP "DATA" $ pure (DATA_ DATAData)
        rsetP = cmdP "RSET" $ pure (RSET_ RSETData)
        noopP = cmdP "NOOP" $ pure (NOOP_ NOOPData)
        quitP = cmdP "QUIT" $ pure (QUIT_ QUITData)
        -- Why are we parsing an argument and discarding it?
        --
        -- VRFY is so dumb that it's worth ensuring it can't be implemented by mistake.
        vrfyP = cmdP "VRFY" $ VRFY_ VRFYData <$ arg clientName

        dataEndP = ("\n.\n" <|> "\r\n.\r\n") $> DATAEnd_ DATAEndData

        dataLineP = DATALine_ . DATALineData <$> (skipSpace *> P.takeWhile (\c -> c /= '\r' && c /= '\n') <* ( ( () <$ lookAhead dataEndP ) <|> endOfLine) )

--

data SMTPResponse = SystemStatus BS.ByteString
                  | HelpMessage BS.ByteString
                  | ServiceReady BS.ByteString
                  | ServiceClosingChannel BS.ByteString
                  | MailActionCompleted BS.ByteString
                  | WillForward BS.ByteString
                  | CannotVRFY BS.ByteString

                  | StartMailInput BS.ByteString

                  | ServiceNotAvailable BS.ByteString
                  | MailboxTempUnavailable BS.ByteString
                  | LocalError BS.ByteString
                  | OutOfSpace BS.ByteString
                  | UnacceptableParams BS.ByteString

                  | UnknownCommand BS.ByteString
                  | ParamSyntaxError BS.ByteString
                  | CommandNotImplemented BS.ByteString
                  | BadCommandSequence BS.ByteString
                  | ParamNotImplemented BS.ByteString
                  | MailboxPermUnavailable BS.ByteString
                  | UserNotLocal BS.ByteString
                  | ExceededStorageQuota BS.ByteString
                  | InvalidMailboxName BS.ByteString
                  | TransactionFailed BS.ByteString
                  | UnknownAddressParams BS.ByteString
                  deriving (Eq, Show)

responseToBS :: SMTPResponse -> BS.ByteString
responseToBS (SystemStatus msg) = "211 " <> msg <> "\r\n"
responseToBS (HelpMessage msg) = "214 " <> msg <> "\r\n"
responseToBS (ServiceReady msg) = "220 " <> msg <> "\r\n"
responseToBS (ServiceClosingChannel msg) = "221 " <> msg <> "\r\n"
responseToBS (MailActionCompleted msg) = "250 " <> msg <> "\r\n"
responseToBS (WillForward msg) = "251 " <> msg <> "\r\n"
responseToBS (CannotVRFY msg) = "252 " <> msg <> "\r\n"

responseToBS (StartMailInput msg) = "354 " <> msg <> "\r\n"

responseToBS (ServiceNotAvailable msg) = "421 " <> msg <> "\r\n"
responseToBS (MailboxTempUnavailable msg) = "450 " <> msg <> "\r\n"
responseToBS (LocalError msg) = "451 " <> msg <> "\r\n"
responseToBS (OutOfSpace msg) = "452 " <> msg <> "\r\n"
responseToBS (UnacceptableParams msg) = "455 " <> msg <> "\r\n"

responseToBS (UnknownCommand msg) = "500 " <> msg <> "\r\n"
responseToBS (ParamSyntaxError msg) = "501 " <> msg <> "\r\n"
responseToBS (CommandNotImplemented msg) = "502 " <> msg <> "\r\n"
responseToBS (BadCommandSequence msg) = "503 " <> msg <> "\r\n"
responseToBS (ParamNotImplemented msg) = "504 " <> msg <> "\r\n"
responseToBS (MailboxPermUnavailable msg) = "550 " <> msg <> "\r\n"
responseToBS (UserNotLocal msg) = "551 " <> msg <> "\r\n"
responseToBS (ExceededStorageQuota msg) = "552 " <> msg <> "\r\n"
responseToBS (InvalidMailboxName msg) = "553 " <> msg <> "\r\n"
responseToBS (TransactionFailed msg) = "554 " <> msg <> "\r\n"
responseToBS (UnknownAddressParams msg) = "555 " <> msg <> "\r\n"
