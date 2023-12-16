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
import Data.Bool
import Data.Text (Text)
import Network.WebSockets

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.Socket.Example
-- >>> import Data.ByteString.Lazy qualified as BS

-- FIXME: reproduce this:
-- *** Exception: writev: resource vanished (Broken pipe)

-- | client that sends a list of Text and returns the responses (via an IORef), dumping Control Messages to stdout
--
-- > refClient_ showStdout fromStdin
-- *** Exception: Network.Socket.connect: <socket: 12>: does not exist (Connection refused)
refClient_ :: (WebSocketsData a, Show a) => Committer IO (String, ConnectionException) -> Emitter IO a -> IO [a]
refClient_ cLog e = do
  (c, r) <- refCommitter
  putStrLn =<< show <$> duplexClient_ defaultSocketConfig (Box c e) cLog
  r

-- | client that sends a list of Text and returns the responses (via an IORef)
--
-- > refClient fromStdin
-- *** Exception: Network.Socket.connect: <socket: 12>: does not exist (Connection refused)
refClient :: (WebSocketsData a, Show a) => Emitter IO a -> IO [a]
refClient e = do
  (c, r) <- refCommitter
  duplexClient defaultSocketConfig (Box c e)
  r

-- | A server that sends text and a client that receives this.
--
-- >>> senderExample ["a","b"]
-- ["a","b"]
senderExample :: [Text] -> IO [Text]
senderExample ts = do
  (c, r) <- refCommitter
  a <- async . serverApp defaultSocketConfig . serve . simplex <$|> (qList ts)
  sleep 0.1
  duplexClient defaultSocketConfig (Box c mempty)
  sleep 0.1
  cancel a
  r

-- | resonderF|duplexClient test
-- the code starts a server in a thread, starts the client in the main thread, and cancels the server thread on completion.
--
-- > responder_Example ["a","b","q","c"]
-- (True,True)
-- (["echo: a","echo: b"],[("receiver duplex:duplexClient_:",CloseRequest 1000 "echoQ quit"),("responseF|receive",CloseRequest 1000 "echoQ quit")])
responder_Example :: [Text] -> IO ([Text], [(String, ConnectionException)])
responder_Example ts = do
  (cLog, rLog) <- refCommitter
  a <- async
    (serverApp defaultSocketConfig
    (serve (responderF_ (echoQ "echo: " "q") cLog)))
  sleep 0.1
  r <- refClient_ cLog <$|> (qList ts)
  sleep 0.1
  cancel a
  (,) <$> pure r <*> rLog

echoQ :: Text -> Text -> Text -> Either Text Text
echoQ prefix q x =
  bool
    (Right $ prefix <> x)
    (Left $ "echoQ quit")
    (x == q)

-- | resonderF|duplexClient test
-- the code starts a server in a thread, starts the client in the main thread, and cancels the server thread on completion.
--
-- The logged exceptions (via a separate committer) should be two CloseRequests on either side of the socket.
-- > responderExample ["a","b","q"]
-- ["echo: a","echo: b"]
responderExample :: [Text] -> IO [Text]
responderExample ts = do
  a <- async
    (serverApp defaultSocketConfig
    (serve (responderF (echoQ "echo: " "q"))))
  sleep 0.1
  r <- refClient <$|> (qList ts)
  sleep 0.1
  cancel a
  pure r

clientIO :: IO ()
clientIO = clientApp defaultSocketConfig (duplexClose (Box toStdout (cancelQ fromStdin)))

serverIO :: IO ()
serverIO = serverApp defaultSocketConfig (serve (duplexClose (Box toStdout (cancelQ fromStdin))))

cancelQ :: Emitter IO Text -> Emitter IO Text
cancelQ e = Emitter $ do
  a <- emit e
  case a of
    Nothing -> pure Nothing
    Just "q" -> pure Nothing
    Just a' -> pure (Just a')
