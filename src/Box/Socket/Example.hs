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
import Control.Concurrent.Classy.Async as C
import Data.Bool
import Data.Text (pack, Text)
import Data.Functor.Contravariant

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
  (c, r) <- refCommitter
  runClient
    defaultSocketConfig
    ( \conn ->
        (\b -> clientApp b conn)
          <$|> ( Box c
                   <$> qList (xs <> ["q"])
               )
    )
  r

tClientIO :: [Text] -> IO ()
tClientIO xs =
  (runClient defaultSocketConfig . clientApp)
    <$|> (Box (contramap (pack . show) toStdout) <$> qList (xs <> ["q"]))

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
