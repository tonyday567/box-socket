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
import Box.Socket.Types
import Control.Concurrent.Async
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Data.Profunctor

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.TCP.Example
-- >>> import Control.Concurrent.Async

-- | A server that only sends and a client that only receives.
--
-- >>> senderExample ["a","b"]
-- ["ab"]
senderExample :: [ByteString] -> IO [ByteString]
senderExample ts = do
  (c, r) <- refCommitter
  a <- async (serverBox defaultTCPConfig (CloseAfter 0.2) . Box mempty <$|> qList ts)
  sleep 0.2
  clientBox defaultTCPConfig (CloseAfter 0.5) (Box c mempty)
  sleep 0.6
  cancel a
  r

-- | A server that only sends and a client that only receives.
--
-- >>> senderLinesExample ["a","b"]
-- ["a","b"]
senderLinesExample :: [Text] -> IO [Text]
senderLinesExample ts = do
  (c, r) <- refCommitter
  a <- async (serverBox defaultTCPConfig (CloseAfter 0.2) . (fromLineBox "\n") . Box mempty <$|> qList ts)
  sleep 0.2
  clientBox defaultTCPConfig (CloseAfter 0.5) (fromLineBox "\n" $ Box c mempty)
  sleep 0.6
  cancel a
  r

-- | echo server example
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

-- | "q" to close the client, reads and writes from std
--
-- >>> clientIO
-- *** Exception: Network.Socket.connect: <socket: ...>: does not exist (Connection refused)
clientIO :: IO ()
clientIO =
  clientBox defaultTCPConfig (CloseAfter 0) (dimap decodeUtf8Lenient encodeUtf8 (stdBox "q"))

-- | "q" to close a client socket down. Ctrl-c to close the server. Reads and writes from std.
--
-- >>> a <- async serverIO
-- >>> serverIO
-- *** Exception: Network.Socket.bind: resource busy (Address already in use)
--
-- >>> cancel a
--
serverIO :: IO ()
serverIO = serverBox defaultTCPConfig (CloseAfter 0) (dimap decodeUtf8Lenient encodeUtf8 (stdBox "q"))
