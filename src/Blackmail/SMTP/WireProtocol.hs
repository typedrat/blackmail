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
msgP = heloP <|> ehloP <|> mailP <|> rcptP <|> dataP <|> rsetP <|> noopP <|> quitP <|> vrfyP <|> startTlsP <|> dataEndP <|> dataLineP
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
                           <* (stringCI "BODY=7BIT" <|> stringCI "BODY=8BITMIME" <|> stringCI "SMTPUTF8" <|> stringCI "SIZE=" <* decimal) `sepBy` (some $ char ' ')
        rcptP = cmdP "RCPT" $ argL "TO:"
                           *> arg (RCPT_ . RCPTData <$> addressP)
        dataP = cmdP "DATA" $ pure (DATA_ DATAData)
        rsetP = cmdP "RSET" $ pure (RSET_ RSETData)
        noopP = cmdP "NOOP" $ pure (NOOP_ NOOPData)
        quitP = cmdP "QUIT" $ pure (QUIT_ QUITData)
        -- Why are we parsing an argument and discarding it?
        --
        -- VRFY is so dumb that it's worth ensuring it can't be implemented by mistake.
        vrfyP = cmdP "VRFY" $ VRFY_ VRFYData <$ (arg clientName *> optional (stringCI "SMTPUTF8"))

        startTlsP = cmdP "STARTTLS" $ pure (STARTTLS_ STARTTLSData)

        dataEndP = ("\n.\n" <|> "\r\n.\r\n") $> DATAEnd_ DATAEndData

        dataLineP = DATALine_ . DATALineData <$> (skipSpace *> P.takeWhile (\c -> c /= '\r' && c /= '\n') <* ( ( () <$ lookAhead dataEndP ) <|> endOfLine) )

--

data SMTPResponse = SystemStatus [BS.ByteString]
                  | HelpMessage [BS.ByteString]
                  | ServiceReady [BS.ByteString]
                  | ServiceClosingChannel [BS.ByteString]
                  | MailActionCompleted [BS.ByteString]
                  | WillForward [BS.ByteString]
                  | CannotVRFY [BS.ByteString]

                  | StartMailInput [BS.ByteString]

                  | ServiceNotAvailable [BS.ByteString]
                  | MailboxTempUnavailable [BS.ByteString]
                  | LocalError [BS.ByteString]
                  | OutOfSpace [BS.ByteString]
                  | UnacceptableParams [BS.ByteString]

                  | UnknownCommand [BS.ByteString]
                  | ParamSyntaxError [BS.ByteString]
                  | CommandNotImplemented [BS.ByteString]
                  | BadCommandSequence [BS.ByteString]
                  | ParamNotImplemented [BS.ByteString]
                  | MailboxPermUnavailable [BS.ByteString]
                  | UserNotLocal [BS.ByteString]
                  | ExceededStorageQuota [BS.ByteString]
                  | InvalidMailboxName [BS.ByteString]
                  | TransactionFailed [BS.ByteString]
                  | UnknownAddressParams [BS.ByteString]
                  deriving (Eq, Show)

mkResponse :: Int -> [BS.ByteString] -> BS.ByteString
mkResponse code msgs = BS.intercalate "\r\n" (reverse resp) <> "\r\n"
    where
        code' = BS.pack (show code)
        (m:ms) = reverse msgs
        resp = (code' <> " " <> m : fmap ((code' <> "-") <>) ms)

responseToBS :: SMTPResponse -> BS.ByteString
responseToBS (SystemStatus msgs) = mkResponse 211 msgs
responseToBS (HelpMessage msgs) = mkResponse 214 msgs
responseToBS (ServiceReady msgs) = mkResponse 220 msgs
responseToBS (ServiceClosingChannel msgs) = mkResponse 221 msgs
responseToBS (MailActionCompleted msgs) = mkResponse 250 msgs
responseToBS (WillForward msgs) = mkResponse 251 msgs
responseToBS (CannotVRFY msgs) = mkResponse 252 msgs

responseToBS (StartMailInput msgs) = mkResponse 354 msgs

responseToBS (ServiceNotAvailable msgs) = mkResponse 421 msgs
responseToBS (MailboxTempUnavailable msgs) = mkResponse 450 msgs
responseToBS (LocalError msgs) = mkResponse 451 msgs
responseToBS (OutOfSpace msgs) = mkResponse 452 msgs
responseToBS (UnacceptableParams msgs) = mkResponse 455 msgs

responseToBS (UnknownCommand msgs) = mkResponse 500 msgs
responseToBS (ParamSyntaxError msgs) = mkResponse 501 msgs
responseToBS (CommandNotImplemented msgs) = mkResponse 502 msgs
responseToBS (BadCommandSequence msgs) = mkResponse 503 msgs
responseToBS (ParamNotImplemented msgs) = mkResponse 504 msgs
responseToBS (MailboxPermUnavailable msgs) = mkResponse 550 msgs
responseToBS (UserNotLocal msgs) = mkResponse 551 msgs
responseToBS (ExceededStorageQuota msgs) = mkResponse 552 msgs
responseToBS (InvalidMailboxName msgs) = mkResponse 553 msgs
responseToBS (TransactionFailed msgs) = mkResponse 554 msgs
responseToBS (UnknownAddressParams msgs) = mkResponse 555 msgs
