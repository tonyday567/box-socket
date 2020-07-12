{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Box.Socket
  ( SocketConfig(..),
    defaultSocketConfig,
    runClient,
    runServer,
    connect,
    clientApp,
    responderApp,
    serverApp,
    serveBox,
    serveBox',
    receiver',
    receiver,
    sender,
    responder,
  )
where

import qualified Network.WebSockets as WS
import Box
import Control.Lens
import NumHask.Prelude hiding (bracket)
import Data.Generics.Labels ()
import Control.Monad.Conc.Class as C
import Control.Monad.Catch
import qualified Control.Concurrent.Classy.Async as C
import Network.Wai.Handler.WebSockets
import Web.Page
import Web.Scotty
import Network.Wai

data SocketConfig
  = SocketConfig
      { host :: Text,
        port :: Int,
        path :: Text
      }
  deriving (Show, Eq, Generic)

defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/"

runClient :: (MonadIO m) => SocketConfig -> WS.ClientApp () -> m ()
runClient c app = liftIO $ WS.runClient (unpack $ c ^. #host) (c ^. #port) (unpack $ c ^. #path) app

runServer :: (MonadIO m) => SocketConfig -> WS.ServerApp -> m ()
runServer c app = liftIO $ WS.runServer (unpack $ c ^. #host) (c ^. #port) app

connect :: (MonadMask m, MonadIO m) => WS.PendingConnection -> Cont m WS.Connection
connect p = Cont $ \action ->
    bracket
      (liftIO $ WS.acceptRequest p)
      (\conn -> liftIO $ WS.sendClose conn ("Bye from connect!" :: Text))
      action

clientApp :: (MonadIO m, MonadConc m) =>
  Box m (Either Text Text) Text ->
  WS.Connection ->
  m ()
clientApp (Box c e) conn =
  void $
    C.race
      (receiver' c conn)
      (sender (Box mempty e) conn)

responderApp ::
  (Text -> Either Text Text) ->
  WS.PendingConnection ->
  IO ()
responderApp f p = with (connect p) (responder f mempty)

serverApp ::
  (MonadConc m, MonadIO m) =>
  Box m Text Text ->
  WS.PendingConnection ->
  m ()
serverApp (Box c e) p = void $ with (connect p)
  (\conn -> C.race
    (receiver c conn)
    (sender (Box mempty e) conn))

serveBox :: SocketConfig -> Page -> Box IO Text Text -> IO ()
serveBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

serveBox' :: SocketConfig -> Page -> Middleware -> Box IO Text Text -> IO ()
serveBox' cfg p m b =
  scotty (cfg ^. #port) $ do
    middleware $ m
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

-- | default websocket receiver
-- Lefts are info/debug
receiver' :: (MonadIO m) =>
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
          commit c $ Left $ "receiver: received: " <> (WS.fromDataMessage msg' :: Text)
          _ <- commit c (Right (WS.fromDataMessage msg'))
          go

-- | default websocket receiver
-- Lefts are info/debug
receiver :: (MonadIO m) =>
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

-- | default websocket sender
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
      commit c $ "sender: sending: " <> (show msg' :: Text)
      liftIO $ WS.sendTextData conn msg'

-- | a receiver that responds based on received Text.
-- lefts are quit signals. Rights are response text.
responder :: (MonadIO m) =>
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
          commit c "responder: normal close"
          liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          case (f $ WS.fromDataMessage msg') of
            Left _ -> do
              commit c "responder: sender initiated close"
              liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
            Right r -> do
              commit c ("responder: sending" <> r)
              liftIO $ WS.sendTextData conn r
              go
