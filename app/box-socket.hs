{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

{-

It's a box. It's a socket. It's an app.

-}

module Main where

import Box.Socket.Example
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
