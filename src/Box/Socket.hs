{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | Websocket components built with 'Box'es.
module Box.Socket
  ( SocketConfig (..),
    defaultSocketConfig,
    runClient,
    runServer,
    connect,
    clientApp,
    responderApp,
    serverApp,
    receiver',
    receiver,
    sender,
    responder,
  )
where

import Box
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.Text (Text, pack, unpack)
import GHC.Generics
import qualified Network.WebSockets as WS

-- | Socket configuration
--
-- >>> defaultSocketConfig
-- SocketConfig {host = "127.0.0.1", port = 9160, path = "/"}
data SocketConfig = SocketConfig
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq, Generic)

-- | official default
defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/"

-- | Run a client app.
runClient :: SocketConfig -> WS.ClientApp () -> IO ()
runClient c app =  WS.runClient (unpack $ host c) (port c) (unpack $ path c) app

-- | Run a server app.
runServer :: SocketConfig -> WS.ServerApp -> IO ()
runServer c app =  WS.runServer (unpack $ host c) (port c) app

-- | Connection continuation.
connect :: WS.PendingConnection -> Codensity IO WS.Connection
connect p = Codensity $ \action ->
  bracket
    ( WS.acceptRequest p)
    (\conn ->  WS.sendClose conn ("Bye from connect!" :: Text))
    ( \conn ->
        withAsync
          ( forever $ WS.sendPing conn ("ping" :: BS.ByteString) >> sleep 30)
          (\_ -> action conn)
    )

-- | A simple client app for a box with Left debug messages.
clientApp ::
  Box IO (Either Text Text) Text ->
  WS.Connection ->
  IO ()
clientApp (Box c e) conn =
  void $
    race
      (receiver' c conn)
      (sender (Box mempty e) conn)

-- | Canned response function.
responderApp ::
  (Text -> Either Text Text) ->
  WS.PendingConnection ->
  IO ()
responderApp f p = process (responder f mempty) (connect p)

-- | Standard server app for a box.
serverApp ::
  Box IO Text Text ->
  WS.PendingConnection ->
  IO ()
serverApp (Box c e) p =
  void $
    process
      ( \conn ->
          race
            (receiver c conn)
            (sender (Box mempty e) conn)
      )
      (connect p)

-- | default websocket receiver with messages
-- Lefts are info/debug
receiver' ::
  Committer IO (Either Text Text) ->
  WS.Connection ->
  IO Bool
receiver' c conn = go
  where
    go = do
      msg <-  WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close w b) ->
          commit
            c
            ( Left
                ( "receiver: received: close: " <> (pack . show) w <> " " <> (pack . show) b
                )
            )
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          _ <- commit c $ Left $ "receiver: received: " <> (WS.fromDataMessage msg' :: Text)
          _ <- commit c (Right (WS.fromDataMessage msg'))
          go

-- | Receiver that only commits.
receiver ::
  Committer IO Text ->
  WS.Connection ->
  IO ()
receiver c conn = go
  where
    go = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> pure ()
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> commit c (WS.fromDataMessage msg') >> go

-- | Sender that only emits.
sender ::
  (WS.WebSocketsData a, Show a) =>
  Box IO Text a ->
  WS.Connection ->
  IO ()
sender (Box c e) conn = forever $ do
  msg <- emit e
  case msg of
    Nothing -> pure ()
    Just msg' -> do
      _ <- commit c $ "sender: sending: " <> ((pack . show) msg' :: Text)
      WS.sendTextData conn msg'

-- | A receiver that responds based on received Text.
-- lefts are quit signals. Rights are response text.
responder ::
  (Text -> Either Text Text) ->
  Committer IO Text ->
  WS.Connection ->
  IO ()
responder f c conn = go
  where
    go = do
      msg <-  WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          _ <- commit c "responder: normal close"
          WS.sendClose conn ("received close signal: responder closed." :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          case f $ WS.fromDataMessage msg' of
            Left _ -> do
              _ <- commit c "responder: sender initiated close"
              WS.sendClose conn ("received close signal: responder closed." :: Text)
            Right r -> do
              _ <- commit c ("responder: sending" <> r)
              WS.sendTextData conn r
              go
