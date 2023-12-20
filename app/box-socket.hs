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

import Box.Socket.Example as Socket
import Box.TCP.Example as TCP
import Options.Applicative

boxSocketOpts :: ParserInfo Opts
boxSocketOpts =
  info
    (boxSocketOptions <**> helper)
    (fullDesc <> progDesc "box-socket tests" <> header "examples of box socket usage")

data SocketType = TCPSocket | WebSocket deriving (Eq, Read, Show)

data ExampleType = ClientIO | ServerIO | EchoExample | SenderExample deriving (Eq, Read, Show)

data Opts = Opts
  { socketType :: SocketType,
    exampleType :: ExampleType
  }
  deriving (Eq, Show)

boxSocketOptions :: Parser Opts
boxSocketOptions =
  Opts
    <$> parseSocketType
    <*> parseExampleType

parseSocketType :: Parser SocketType
parseSocketType =
  flag' WebSocket (long "websocket" <> help "websocket socket")
    <|> flag' TCPSocket (long "tcp" <> help "tcp socket")
    <|> pure WebSocket

parseExampleType :: Parser ExampleType
parseExampleType =
  flag' ClientIO (long "clientio" <> help "client socket")
    <|> flag' ServerIO (long "serverio" <> help "server socket")
    <|> flag' EchoExample (long "echo" <> help "responder example")
    <|> flag' SenderExample (long "sender" <> help "sender example")
    <|> pure EchoExample

main :: IO ()
main = do
  o <- execParser boxSocketOpts
  r <- case (socketType o, exampleType o) of
    (WebSocket, ClientIO) -> show <$> Socket.clientIO
    (WebSocket, ServerIO) -> show <$> Socket.serverIO
    (WebSocket, SenderExample) -> show <$> Socket.senderExample ["hi", "bye"]
    (WebSocket, EchoExample) -> show <$> Socket.echoExample ["hi","bye"]
    (TCPSocket, ClientIO) -> show <$> TCP.clientIO
    (TCPSocket, ServerIO) -> show <$> TCP.serverIO
    (TCPSocket, SenderExample) -> show <$> TCP.senderExample ["hi", "bye"]
    (TCPSocket, EchoExample) -> show <$> TCP.echoExample ["hi","bye"]
  putStrLn r
