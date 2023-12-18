{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{-

It's a box. It's a socket. It's an example.

-}

module Box.Socket.Example where

import Box
import Box.Socket
import Control.Concurrent.Async
import Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.Socket.Example
-- >>> import Data.ByteString.Lazy qualified as BS
-- >>> import Control.Concurrent.Async

-- | A server that only sends and a client that only receives.
--
-- >>> senderExample ["a","b"]
-- ["a","b"]
senderExample :: [Text] -> IO [Text]
senderExample ts = do
  (c, r) <- refCommitter
  a <- async (serverBox defaultSocketConfig (CloseAfter 0.2) . Box mempty <$|> qList ts)
  sleep 0.1
  clientBox defaultSocketConfig StayOpen (Box c mempty)
  sleep 0.1
  cancel a
  r

-- | echo server example
--
-- >>> echoExample ["a","b","c"]
-- ["echo: a","echo: b","echo: c"]
echoExample :: [Text] -> IO [Text]
echoExample ts = do
  (c, r) <- refCommitter
  a <- async
    (responseServer defaultSocketConfig (pure . (("echo: " :: Text) <>)))
  sleep 0.1
  clientBox defaultSocketConfig (CloseAfter 0.5) . Box c <$|> qList ts
  sleep 0.1
  cancel a
  r

-- | Box that emits from and commits to std
ioBox :: Box IO Text Text
ioBox = Box toStdout (takeUntilE (=="q") fromStdin)

-- | "q" to close the client, reads and writes from std
--
-- >>> clientIO
-- *** Exception: Network.Socket.connect: <socket: ...>: does not exist (Connection refused)
clientIO :: IO ()
clientIO =
  clientBox defaultSocketConfig (CloseAfter 0) ioBox

-- | "q" to close a client socket down. Ctrl-c to close the server. Reads and writes from std.
--
-- >>> a <- async serverIO
-- >>> serverIO
-- *** Exception: Network.Socket.bind: resource busy (Address already in use)
--
-- >>> cancel a
--
serverIO :: IO ()
serverIO = serverBox defaultSocketConfig (CloseAfter 0) ioBox
