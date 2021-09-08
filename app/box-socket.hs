{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-

It's a box. It's a socket.

-}

module Main where

import Box
import Box.Socket
import Control.Concurrent.Classy.Async as C
import Control.Lens hiding (Unwrapped, Wrapped)
import Data.Bool
import Data.Generics.Labels ()
import Data.Text (pack)
import Options.Generic

data SocketType = Client | Responder | TestRun deriving (Eq, Read, Show, Generic)

instance ParseField SocketType

instance ParseRecord SocketType

instance ParseFields SocketType

newtype Opts w = Opts
  { apptype :: w ::: SocketType <?> "type of websocket app"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "example websocket apps"
  r :: String <- case apptype o of
    Client -> show <$> clientIO
    Responder -> show <$> q' serverIO
    TestRun -> show <$> testRun
  putStrLn r

-- * older stuff

serverIO :: IO ()
serverIO =
  runServer
    defaultSocketConfig
    (responderApp (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x == "q")))

clientIO :: IO ()
clientIO =
  (runClient defaultSocketConfig . clientApp)
    (Box (contramap (pack . show) toStdout) fromStdin)

q' :: IO a -> IO (Either () a)
q' f = C.race (cancelQ fromStdin) f

cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    _notQ -> do
      putStrLn "nothing happens"
      cancelQ e

-- | test of clientApp via a cRef committer and a canned list of Text
tClient :: [Text] -> IO [Either Text Text]
tClient xs = do
  (c, r) <- cRef
  runClient
    defaultSocketConfig
    ( \conn ->
        (\b -> clientApp b conn)
          <$.> ( Box c
                   <$> fromListE (xs <> ["q"])
               )
    )
  r

tClientIO :: [Text] -> IO ()
tClientIO xs =
  (runClient defaultSocketConfig . clientApp)
    <$.> (Box (contramap (pack . show) toStdout) <$> fromListE (xs <> ["q"]))

-- | main test run of client-server functionality
-- the code starts a server in a thread, starts the client in the main thread, and cancels the server on completion.
-- >>> testRun
-- [Left "receiver: received: echo:1",Right "echo:1",Left "receiver: received: echo:2",Right "echo:2",Left "receiver: received: echo:3",Right "echo:3",Left "receiver: received: close: 1000 \"received close signal: responder closed.\""]
testRun :: IO [Either Text Text]
testRun = do
  a <- C.async (runServer defaultSocketConfig (responderApp (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x == "q"))))
  sleep 0.1
  r <- tClient (pack . show <$> [1 .. 3 :: Int])
  C.cancel a
  pure r
