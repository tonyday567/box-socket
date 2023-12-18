{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | Websocket components built with 'Box'es.
module Box.Socket
  ( SocketConfig (..),
    defaultSocketConfig,
    PostSend (..),
    SocketStatus (..),
    connect,
    serve,
    pending,
    receiver,
    receiver_,
    sender,
    sender_,
    duplex,
    duplex_,
    simplex,
    clientBox,
    clientCoBox,
    serverBox,
    serverCoBox,
    responseServer,
  )
where

import Box
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString qualified as BS
import Data.Text (Text, pack, unpack)
import GHC.Generics ( Generic )
import Network.WebSockets
import Control.Exception

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

-- | connect an action (ie a client)
connect :: SocketConfig -> Codensity IO Connection
connect c = Codensity $ \action ->
  runClient (unpack $ host c) (port c) (unpack $ path c) action

-- | serve an action (ie a server)
serve :: SocketConfig -> Codensity IO Connection
serve c = Codensity $
  runServerWithOptions (defaultServerOptions { serverHost = (unpack $ host c), serverPort = port c}) . upgrade
  where
    upgrade action p = void $ action <$|> pending p

-- | Given a PendingConnection, provide a Connection continuation.
pending:: PendingConnection -> Codensity IO Connection
pending p = Codensity $ \action ->
  bracket
    (acceptRequest p)
    -- FIXME: is this just-in-case ok?
    (\conn -> sendClose conn ("connect close" :: Text))
    ( \conn ->
        withAsync
          (forever $ sendPing conn ("connect ping" :: BS.ByteString) >> sleep 30)
          (\_ -> action conn)
    )

-- | Commit received messages, finalising on receiving a CloseRequest
receiver ::
  (WebSocketsData a) =>
  Committer IO a ->
  Connection ->
  IO ()
receiver c conn = go
  where
    go = do
      msg <- try (receiveData conn)
      case msg of
        Left (CloseRequest _ _) -> pure ()
        Left err -> throwIO err
        Right msg' -> commit c msg' >> go

-- | Commit received messages, finalising on receiving a CloseRequest
receiver_ ::
  (WebSocketsData a, Show a) =>
  Committer IO a ->
  Committer IO Text ->
  Connection ->
  IO ()
receiver_ c cLog conn = go
  where
    go = do
      msg <- try (receiveData conn)
      _ <- commit cLog ("receiver_:" <> pack (show msg))
      case msg of
        Left (CloseRequest _ _) -> pure ()
        Left err -> throwIO err
        Right msg' -> commit c msg' >> go

-- | Whether a socket remains open or closed after an action finishes.
data SocketStatus = SocketOpen | SocketClosed | SocketBroken deriving (Generic, Eq, Show)

-- | Send emitted messages, returning whether the socket remained open (the emitter ran out of emits) or closed (a CloseRequest was received).
sender ::
  (WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO SocketStatus
sender e conn = go
  where
    go = do
      msg <- emit e
      case msg of
        Nothing -> pure SocketOpen
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          case ok of
            Left (CloseRequest _ _) -> pure SocketClosed
            Left err -> throwIO err
            Right () -> go

-- | Send emitted messages, returning whether the socket remained open (the emitter ran out of emits) or closed (a CloseRequest was received).
sender_ ::
  (WebSocketsData a, Show a) =>
  Emitter IO a ->
  Committer IO Text ->
  Connection ->
  IO SocketStatus
sender_ e cLog conn = go
  where
    go = do
      msg <- emit e
      _ <- commit cLog ("emit sender_:" <> pack (show msg))
      case msg of
        Nothing -> pure SocketOpen
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          _ <- commit cLog ("send sender_:" <> pack (show ok))
          case ok of
            Left (CloseRequest _ _) -> pure SocketClosed
            Left err -> throwIO err
            Right () -> go

data PostSend = StayOpen | CloseAfter Double deriving (Generic, Eq, Show)

-- | A two-way connection. Closes if it receives a CloseRequest exception, or if PostSend is CloseAfter.
duplex::
  (WebSocketsData a) =>
  PostSend ->
  Box IO a a ->
  Connection ->
  IO ()
duplex ps (Box c e) conn = do
  concurrentlyRight
    (do
        status <- sender e conn
        case (ps, status) of
          (CloseAfter s, SocketOpen) -> do
            sleep s
            (sendClose conn ("close after sending" :: Text))
          _ -> pure ())
    (receiver c conn)

-- | A two-way connection. Closes if it receives a CloseRequest exception, or if PostSend is CloseAfter.
duplex_ ::
  (WebSocketsData a, Show a) =>
  PostSend ->
  Committer IO Text ->
  Box IO a a ->
  Connection ->
  IO ()
duplex_ ps cLog (Box c e) conn = do
  concurrentlyRight
    (do
        status <- sender_ e cLog conn
        _ <- commit cLog ("duplex_:sender_ closed with" <> pack (show status))
        case (ps, status) of
          (CloseAfter s, SocketOpen) -> do
            sleep s
            (sendClose conn ("close after sending" :: Text))
          _ -> pure ())
    (do
       receiver_ c cLog conn
       void $ commit cLog ("duplex_:receiver_ closed")
       )
  void $ commit cLog ("duplex_ closed")

-- | A one-way sender. Underneath, it still needs to listen and receive a CloseRequest.
simplex ::
  (WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO ()
simplex e conn = do
  duplex (CloseAfter 0.2) (Box mempty e) conn

-- | A Box action for a socket client.
clientBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  Box IO a a ->
  IO ()
clientBox cfg ps b = duplex ps b <$|> connect cfg

-- | A socket client CoBox.
clientCoBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  CoBox IO a a
clientCoBox cfg ps = fromAction (clientBox cfg ps)

-- | A Box action for a socket server.
serverBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  Box IO a a ->
  IO ()
serverBox cfg ps b = duplex ps b <$|> serve cfg

-- | A CoBox socket server.
serverCoBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  CoBox IO a a
serverCoBox cfg ps = fromAction (serverBox cfg ps)

-- | A receiver that applies a response function to received Text.
responseServer :: (WebSocketsData a) => SocketConfig -> (a -> Maybe a) -> IO ()
responseServer cfg f = fuse (pure . f) <$|> serverCoBox cfg (CloseAfter 0.5)
