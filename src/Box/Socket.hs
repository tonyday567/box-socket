{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
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
import qualified Control.Concurrent.Classy.Async as C
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Conc.Class as C
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import NumHask.Prelude hiding (bracket)

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
runClient :: (MonadIO m) => SocketConfig -> WS.ClientApp () -> m ()
runClient c app = liftIO $ WS.runClient (unpack $ c ^. #host) (c ^. #port) (unpack $ c ^. #path) app

-- | Run a server app.
runServer :: (MonadIO m) => SocketConfig -> WS.ServerApp -> m ()
runServer c app = liftIO $ WS.runServer (unpack $ c ^. #host) (c ^. #port) app

-- | Connection continuation.
connect :: (MonadIO m, MonadConc m) => WS.PendingConnection -> Cont m WS.Connection
connect p = Cont $ \action ->
  bracket
    (liftIO $ WS.acceptRequest p)
    (\conn -> liftIO $ WS.sendClose conn ("Bye from connect!" :: Text))
    ( \conn ->
        C.withAsync
          (liftIO $ forever $ WS.sendPing conn ("ping" :: ByteString) >> sleep 30)
          (\_ -> action conn)
    )

-- | A simple client app for a box with Left debug messages.
clientApp ::
  (MonadIO m, MonadConc m) =>
  Box m (Either Text Text) Text ->
  WS.Connection ->
  m ()
clientApp (Box c e) conn =
  void $
    C.race
      (receiver' c conn)
      (sender (Box mempty e) conn)

-- | Canned response function.
responderApp ::
  (Text -> Either Text Text) ->
  WS.PendingConnection ->
  IO ()
responderApp f p = with (connect p) (responder f mempty)

-- | Standard server app for a box.
serverApp ::
  (MonadConc m, MonadIO m) =>
  Box m Text Text ->
  WS.PendingConnection ->
  m ()
serverApp (Box c e) p =
  void $
    with
      (connect p)
      ( \conn ->
          C.race
            (receiver c conn)
            (sender (Box mempty e) conn)
      )

-- | default websocket receiver with messages
-- Lefts are info/debug
receiver' ::
  (MonadIO m) =>
  Committer m (Either Text Text) ->
  WS.Connection ->
  m Bool
receiver' c conn = go
  where
    go = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close w b) ->
          commit
            c
            ( Left
                ( "receiver: received: close: " <> show w <> " " <> show b
                )
            )
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          _ <- commit c $ Left $ "receiver: received: " <> (WS.fromDataMessage msg' :: Text)
          _ <- commit c (Right (WS.fromDataMessage msg'))
          go

-- | Receiver that only commits.
receiver ::
  (MonadIO m) =>
  Committer m Text ->
  WS.Connection ->
  m ()
receiver c conn = go
  where
    go = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> pure ()
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> commit c (WS.fromDataMessage msg') >> go

-- | Sender that only emits.
sender ::
  (MonadIO m, WS.WebSocketsData a, Show a) =>
  Box m Text a ->
  WS.Connection ->
  m ()
sender (Box c e) conn = forever $ do
  msg <- emit e
  case msg of
    Nothing -> pure ()
    Just msg' -> do
      _ <- commit c $ "sender: sending: " <> (show msg' :: Text)
      liftIO $ WS.sendTextData conn msg'

-- | A receiver that responds based on received Text.
-- lefts are quit signals. Rights are response text.
responder ::
  (MonadIO m) =>
  (Text -> Either Text Text) ->
  Committer m Text ->
  WS.Connection ->
  m ()
responder f c conn = go
  where
    go = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          _ <- commit c "responder: normal close"
          liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          case f $ WS.fromDataMessage msg' of
            Left _ -> do
              _ <- commit c "responder: sender initiated close"
              liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
            Right r -> do
              _ <- commit c ("responder: sending" <> r)
              liftIO $ WS.sendTextData conn r
              go
