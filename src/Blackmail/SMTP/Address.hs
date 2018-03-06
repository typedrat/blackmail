module Blackmail.SMTP.Address (Address(..), addressP) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

-- There's no support for routes. UUCP and non-resolvable hosts are deep in the ground by now.
-- Likewise, if it fails to receive mail from a client that has flagrant,
-- Windows CE level disregard for the standards, I'll just tell the person trying to email me
-- "Here's a nickle, kid, get yourself a better mail client."

data Address = Address { _mailbox :: BS.ByteString, _domain :: BS.ByteString }
             deriving (Show, Eq)

addressP :: Parser Address
addressP = char '<' *> skipRoute *> ((\m _ d -> Address m d) <$> mailbox <*> char '@' <*> domain) <* char '>'
    where
        skipRoute :: Parser ()
        skipRoute = () <$ optional (char '@' *> takeTill (== ':') *> char ':')

        mboxScan :: (Char, Bool) -> Char -> Maybe (Char, Bool)
        mboxScan (last, True) char
            | last /= '\\' && char == '"' = Just (char, False)
            | otherwise                   = Just (char, True)
        mboxScan (last, False) char
            | last /= '\\' && char == '@' = Nothing
            | last /= '\\' && char == '"' = Just (char, True)
            | otherwise                   = Just (char, False)

        mboxDecode :: BS.ByteString -> BS.ByteString
        mboxDecode = BS.filter (\c -> c /= '\x5c' && c /= '"') -- 0x5C = backspace

        mailbox, domain :: Parser BS.ByteString
        mailbox = mboxDecode <$> scan ('\0', False) mboxScan
        domain = takeWhile1 (\c -> c > ' ' && c /= '>' && c /= '\DEL')
