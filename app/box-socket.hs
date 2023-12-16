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

data SocketType = ClientIO | ServerIO | ResponderExample | SenderExample deriving (Eq, Read, Show)

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
  flag' ClientIO (long "clientio" <> help "client socket")
    <|> flag' ServerIO (long "serverio" <> help "server socket")
    <|> flag' ResponderExample (long "responder" <> help "responder example")
    <|> flag' SenderExample (long "sender" <> help "sender example")
    <|> pure ResponderExample

main :: IO ()
main = do
  o <- execParser boxSocketOpts
  r <- case socketType o of
    -- FIXME: neither of these close after a "q"
    ClientIO -> show <$> clientIO
    ServerIO -> show <$> serverIO
    -- sender ok
    SenderExample -> show <$> senderExample ["hi", "bye"]
    -- FIXME: hangs
    ResponderExample -> show <$> responderExample ["hi","bye"]
  putStrLn r
