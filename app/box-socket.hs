{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{-

It's a box. It's a socket. It's an app.

-}

module Main where

import Box.Socket.Example
import Options.Applicative

boxSocketOpts :: ParserInfo Opts
boxSocketOpts =
  info
    (boxSocketOptions <**> helper)
    (fullDesc <> progDesc "box-socket tests" <> header "examples of box socket usage")

data SocketType = Client | Responder | TestRun deriving (Eq, Read, Show)

newtype Opts = Opts
  { socketType :: SocketType
  }
  deriving (Eq, Show)

boxSocketOptions :: Parser Opts
boxSocketOptions =
  Opts
    <$> parseSocketType

parseSocketType :: Parser SocketType
parseSocketType =
  flag' Client (long "client" <> help "client socket")
    <|> flag' Responder (long "responder" <> help "responder socket")
    <|> flag' TestRun (long "test" <> help "run the test socket")
    <|> pure TestRun

main :: IO ()
main = do
  o <- execParser boxSocketOpts
  r <- case socketType o of
    Client -> show <$> clientIO
    Responder -> show <$> q' serverIO
    TestRun -> show <$> testRun
  putStrLn r
