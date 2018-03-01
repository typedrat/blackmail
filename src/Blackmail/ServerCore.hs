module Blackmail.ServerCore (ConnectionHandler, runServer) where

import qualified Data.ByteString.Char8 as BS
import Pipes
import Pipes.Safe
import Pipes.Network.TCP.Safe

type ConnectionHandler m = forall r. (Monad m) => (Producer BS.ByteString m (), Consumer BS.ByteString m r, SockAddr) -> m ()
-- ^ The type of a simple connection handler. Just a function from a 'Producer' for the received data, a 'Consumer' for the data
--   to be transmitted, and the 'SockAddr' of the client to a monadic action returning ().

runServer :: (MonadSafe m) => HostPreference -> ServiceName -> ConnectionHandler IO -> m ()
runServer host port server = serve host port $ \(sock, addr) -> do
    let recv = fromSocket sock 4096
    let send = toSocket sock
    server (recv, send, addr)
-- ^ Given a hostname and port to listen on, loop infinitely and handle each new connection with the connection handler.