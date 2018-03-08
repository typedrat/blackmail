module Blackmail.SMTP.Address (Address(..), addressP) where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Megaparsec
import Text.Megaparsec.Byte
import Data.Char (chr, ord)
import Data.Word (Word8)

-- There's no support for routes. UUCP and non-resolvable hosts are deep in the ground by now.
-- Likewise, if it fails to receive mail from a client that has flagrant,
-- Windows CE level disregard for the standards, I'll just tell the person trying to email me
-- "Here's a nickle, kid, get yourself a better mail client."

data Address = Address { _mailbox :: T.Text, _domain :: T.Text }
             deriving (Show, Eq)

ord' :: Char -> Word8
ord' = fromIntegral . ord

chr' :: Word8 -> Char
chr' = chr . fromIntegral

addressP :: (MonadParsec e BS.ByteString m) => m Address
addressP = char' '<' *> skipRoute *> ((\m _ d -> Address m d) <$> mailbox <*> char' '@' <*> domain) <* char' '>'
    where
        skipRoute = () <$ optional (char' '@' *> takeWhileP Nothing ((/= ':') . chr') *> char' ':')

        char' c = char (ord' c)
        backslash = ord' '\\'
        doubleQuote = ord' '"'
        atSymbol = ord' '@'

        mboxScan (last, True) char
            | last /= backslash && char == doubleQuote = Just (char, False)
            | otherwise                                = Just (char, True)
        mboxScan (last, False) char
            | last /= backslash && char == atSymbol    = Nothing
            | last /= backslash && char == doubleQuote = Just (char, True)
            | otherwise                                = Just (char, False)

        mboxDecode = T.filter (\c -> c /= '\\' && c /= '"')

        mailbox = mboxDecode . T.decodeUtf8 <$> scanP Nothing (0, False) mboxScan
        domain = T.decodeUtf8 <$> takeWhile1P Nothing ((\c -> c > ' ' && c /= '>' && c /= '\DEL') . chr')
