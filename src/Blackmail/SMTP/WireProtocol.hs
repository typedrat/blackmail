module Blackmail.SMTP.WireProtocol (msgP, dataBodyP, SMTPResponse(..), responseToText) where

import Control.Applicative
import Control.FSM.Monad
import Data.Attoparsec.ByteString.Char8 as P hiding (skipSpace)
import qualified Data.ByteString.Char8 as BS
import Data.Semigroup

import Blackmail.SMTP.Address
import Blackmail.SMTP.StateMachine.Types

--

msgP :: Parser (EventType SMTP)
msgP = heloP <|> ehloP <|> mailP <|> rcptP <|> dataP <|> rsetP <|> noopP <|> quitP <|> vrfyP <|> unknownP
    where
        cmdP :: BS.ByteString -> Parser (EventType SMTP) -> Parser (EventType SMTP)
        cmdP cmd p = stringCI cmd *> ((p <* space `manyTill` endOfLine) <|> (Invalid_ InvalidData <$ anyChar `manyTill` endOfLine))

        skipSpace :: Parser ()
        skipSpace = skipWhile (\c -> c == ' ' || c == '\t')

        clientName :: Parser BS.ByteString
        clientName = P.takeWhile1 (not . isSpace)

        arg :: Parser a -> Parser a
        arg p = skipSpace *> p <* skipSpace

        argL :: BS.ByteString -> Parser BS.ByteString
        argL s = arg $ stringCI s

        heloP, ehloP, mailP, rcptP, dataP, rsetP, noopP, quitP, vrfyP, unknownP :: Parser (EventType SMTP)
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

        unknownP = Unknown_ . UnknownData <$> P.take 4 <* anyChar `manyTill` endOfLine


dataBodyP :: Parser (EventType SMTP)
dataBodyP = DATABody_ . DATABodyData . BS.unlines <$> P.takeTill (\c -> c == '\r' || c == '\n') `manyTill` (endOfLine *> char '.' *> endOfLine)

--

data SMTPResponse = SystemStatus
                  | HelpMessage
                  | ServiceReady
                  | ServiceClosingChannel
                  | MailActionCompleted
                  | WillForward
                  | CannotVRFY

                  | StartMailInput

                  | ServiceNotAvailable
                  | MailboxTempUnavailable
                  | LocalError
                  | OutOfSpace
                  | UnacceptableParams

                  | UnknownCommand
                  | ParamSyntaxError
                  | CommandNotImplemented
                  | BadCommandSequence
                  | ParamNotImplemented
                  | MailboxPermUnavailable
                  | UserNotLocal
                  | ExceededStorageQuota
                  | InvalidMailboxName
                  | TransactionFailed
                  | UnknownAddressParams
                  deriving (Eq, Show)

responseToText :: SMTPResponse -> BS.ByteString -> BS.ByteString
responseToText resp exp = code resp <> exp <> "\r\n"
    where
        code :: SMTPResponse -> BS.ByteString
        code SystemStatus = "211 "
        code HelpMessage = "214 "
        code ServiceReady = "220 "
        code ServiceClosingChannel = "221 "
        code MailActionCompleted = "250 "
        code WillForward = "251 "
        code CannotVRFY = "252 "

        code StartMailInput = "354 "

        code ServiceNotAvailable = "421 "
        code MailboxTempUnavailable = "450 "
        code LocalError = "451 "
        code OutOfSpace = "452 "
        code UnacceptableParams = "455 "

        code UnknownCommand = "500 "
        code ParamSyntaxError = "501 "
        code CommandNotImplemented = "502 "
        code BadCommandSequence = "503 "
        code ParamNotImplemented = "504 "
        code MailboxPermUnavailable = "550 "
        code UserNotLocal = "551 "
        code ExceededStorageQuota = "552 "
        code InvalidMailboxName = "553 "
        code TransactionFailed = "554 "
        code UnknownAddressParams = "555 "
