{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{-

It's a box. It's a socket. It's an example.

-}

module Box.TCP.Example where

import Box
import Box.TCP
import Box.Types
import Control.Concurrent.Async
import Data.Functor.Contravariant
import Data.ByteString
import Data.Text.Encoding

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.TCP.Example
-- >>> import Control.Concurrent.Async

-- | A server that only sends and a client that only receives.
--
-- FIXME:
--
-- >>> senderExample ["a","b"]
-- []
senderExample :: [ByteString] -> IO [ByteString]
senderExample ts = do
  (c, r) <- refCommitter
  a <- async (serverBox defaultTCPConfig (CloseAfter 0.2) . Box mempty <$|> qList ts)
  sleep 0.1
  clientBox defaultTCPConfig StayOpen (Box c mempty)
  sleep 0.1
  cancel a
  r

-- | echo server example
--
-- FIXME:
--
-- >>> echoExample ["a","b","c"]
-- ["echo: abc"]
echoExample :: [ByteString] -> IO [ByteString]
echoExample ts = do
  (c, r) <- refCommitter
  a <- async
    (responseServer defaultTCPConfig (pure . (("echo: " <>))))
  sleep 0.1
  clientBox defaultTCPConfig (CloseAfter 0.2) . Box c <$|> qList ts
  sleep 0.1
  cancel a
  r

-- | 'Box' that emits from and commits to std, "q" to quit.
ioBox :: Box IO ByteString ByteString
ioBox = Box (contramap decodeUtf8Lenient toStdout) (fmap encodeUtf8 (takeUntilE (=="q") fromStdin))

-- | "q" to close the client, reads and writes from std
--
-- >>> clientIO
-- *** Exception: Network.Socket.connect: <socket: ...>: does not exist (Connection refused)
clientIO :: IO ()
clientIO =
  clientBox defaultTCPConfig (CloseAfter 0) ioBox

-- | "q" to close a client socket down. Ctrl-c to close the server. Reads and writes from std.
--
-- >>> a <- async serverIO
-- >>> serverIO
-- *** Exception: Network.Socket.bind: resource busy (Address already in use)
--
-- >>> cancel a
--
serverIO :: IO ()
serverIO = serverBox defaultTCPConfig (CloseAfter 0) ioBox
