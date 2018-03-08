module Blackmail.SMTP.WireProtocol (commandP, dataBodyP, SMTPResponse(..), responseToText) where

import Control.Applicative
import Control.FSM.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Semigroup
import Text.Megaparsec
import Text.Megaparsec.Byte

import Blackmail.SMTP.Address
import Blackmail.SMTP.StateMachine.Types

--

commandP :: (MonadParsec e BS.ByteString m) => m (EventType SMTP)
commandP = (HELO_ <$> heloP) <|> (EHLO_ <$> ehloP) <|> (MAIL_ <$> mailP) <|> (RCPT_ <$> rcptP) <|> (RSET_ <$> rsetP) <|> (NOOP_ <$> noopP) <|> (QUIT_ <$> quitP) <|> (VRFY_ <$> vrfyP)
    where
        cmdP :: (MonadParsec e BS.ByteString m) => BS.ByteString -> m a -> m a
        cmdP cmd p = string' cmd *> (p <* space `manyTill` eol)

        skipHorizontalSpace :: (MonadParsec e BS.ByteString m) => m ()
        skipHorizontalSpace = () <$ takeWhileP Nothing (\c -> c == 0x20 || c == 0x09)

        clientName :: (MonadParsec e BS.ByteString m) => m BS.ByteString
        clientName = takeWhile1P Nothing (\c -> c > 0x20 && c /= 0x7F)

        arg :: (MonadParsec e BS.ByteString m) => m a -> m a
        arg p = skipHorizontalSpace *> p <* skipHorizontalSpace

        argL :: (MonadParsec e BS.ByteString m) => BS.ByteString -> m BS.ByteString
        argL s = arg $ string' s

        heloP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData HELO)
        heloP = cmdP "HELO" $ HELOData <$> arg clientName

        ehloP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData EHLO)
        ehloP = cmdP "EHLO" $ EHLOData <$> arg clientName

        mailP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData MAIL)
        mailP = cmdP "MAIL" $ argL "FROM:"
                           *> arg (fmap (MAILData . Just) addressP <|> MAILData Nothing <$ string "<>")
                           -- I'm not going to do Q-P conversion, but I have to pretend I care
                           <* optional (string' "BODY=7BIT" <|> string' "BODY=8BITMIME")

        rcptP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData RCPT)
        rcptP = cmdP "RCPT" $ argL "TO:"
                           *> arg (RCPTData <$> addressP)

        dataP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData DATA)
        dataP = cmdP "DATA" $ pure DATAData

        rsetP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData RSET)
        rsetP = cmdP "RSET" $ pure RSETData

        noopP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData NOOP)
        noopP = cmdP "NOOP" $ pure NOOPData

        quitP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData QUIT)
        quitP = cmdP "QUIT" $ pure QUITData

        -- | Why are we parsing an argument and discarding it?
        --
        -- VRFY is so dumb that it's worth ensuring it can't be implemented by mistake.
        vrfyP :: (MonadParsec e BS.ByteString m) => m (SMTPEventData VRFY)
        vrfyP = cmdP "VRFY" $ VRFYData <$ arg clientName

dataBodyP :: (MonadParsec e BS.ByteString m) => m (EventType SMTP)
dataBodyP = DATABody_ . DATABodyData . BS.unlines <$> takeWhileP (Just "message body") (\c -> c /= 0x0D && c /= 0x0A) `manyTill` (eol *> char 0x2E *> eol)

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
