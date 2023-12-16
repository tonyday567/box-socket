{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | Websocket components built with 'Box'es.
module Box.Socket
  ( SocketConfig (..),
    defaultSocketConfig,
    clientApp,
    serverApp,
    connect,
    serve,
    receiver,
    receiver_,
    sender,
    sender_,
    senderClose,
    responderF,
    responderF_,
    duplex,
    duplex_,
    duplex'_,
    duplex',
    duplexClose,
    simplex,
    duplexServer,
    duplexServer_,
    duplexClient,
    duplexClient_,
  )
where

import Box
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString qualified as BS
import Data.Text (Text, unpack)
import GHC.Generics ( Generic )
import Network.WebSockets
import Data.Functor.Contravariant
import Control.Exception
import Data.Bifunctor

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
clientApp :: SocketConfig -> ClientApp a -> IO a
clientApp c app = runClient (unpack $ host c) (port c) (unpack $ path c) app

-- | Run a server app.
serverApp :: SocketConfig -> ServerApp -> IO ()
serverApp c app = runServer (unpack $ host c) (port c) app

-- clientApp defaultSocketConfig (\conn -> sendClose conn ("test"::Text))

-- | Connection continuation.
connect :: PendingConnection -> Codensity IO Connection
connect p = Codensity $ \action ->
  bracket
    (acceptRequest p)
    -- FIXME: is this just-in-case ok?
    (\conn -> sendClose conn ("connect close" :: Text))
    ( \conn ->
        withAsync
          (forever $ sendPing conn ("connect ping" :: BS.ByteString) >> sleep 30)
          (\_ -> action conn)
    )

-- | Commit received messages, and action control messages
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

-- | Commit received messages, and action control messages
receiver_ ::
  (WebSocketsData a, Show a) =>
  Committer IO a ->
  Committer IO ConnectionException ->
  Connection ->
  IO Bool
receiver_ c cErrs conn = go
  where
    go = do
      msg <- try (receiveData conn)
      case msg of
        Left cr@(CloseRequest _ _) -> commit cErrs cr >> putStrLn "receiver_ CR" >> pure True
        Left err -> commit cErrs err >> pure False
        Right msg' -> commit c msg' >> putStrLn (show msg') >> go

-- | Send emitted messages
sender ::
  (WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO Bool
sender e conn = go
  where
    go = do
      msg <- emit e
      case msg of
        Nothing -> pure False
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          case ok of
            Left (CloseRequest _ _) -> pure True
            Left err -> throwIO err
            Right () -> go

-- | Send emitted messages and then close
senderClose ::
  (WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO Bool
senderClose e conn = go
  where
    go = do
      msg <- emit e
      case msg of
        Nothing -> sendClose conn ("senderClose"::Text) >> pure False
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          case ok of
            Left (CloseRequest _ _) -> pure True
            Left err -> throwIO err
            Right () -> go

data ClosureStatus = Closed | UnClosed | Broken deriving (Generic, Eq, Show)

-- | Send emitted messages
sender_ ::
  (WebSocketsData a, Show a) =>
  Emitter IO a ->
  Committer IO ConnectionException ->
  Connection ->
  IO ClosureStatus
sender_ e cExceptions conn = go
  where
    go = do
      msg <- emit e
      case msg of
        Nothing -> putStrLn "sender expired" >> pure UnClosed
        Just msg' -> do
          putStrLn (show msg')
          ok <- try (sendTextData conn msg')
          case ok of
            Left cr@(CloseRequest _ _) -> commit cExceptions cr >> putStrLn "sender_ CR" >> pure Closed
            Left err -> commit cExceptions err >> pure Broken
            Right () -> go

-- | A two-way connection. Closes if receiver or sender sides close.
duplex ::
  (WebSocketsData a) =>
  Box IO a a ->
  Connection ->
  IO ()
duplex (Box c e) conn =
  void $
    concurrently
      (receiver c conn)
      (sender e conn)

-- | A two-way connection. Closes if receiver receives the CloseRequest exception.
duplexClose ::
  (Show a, WebSocketsData a) =>
  Box IO a a ->
  Connection ->
  IO ()
duplexClose (Box c e) conn = do
  b <- concurrentlyRight
    (do
        status <- sender_ e showStdout conn
        when (status == UnClosed) (sendClose conn ("unclosed sender" :: Text)))
    (receiver_ c showStdout conn)
  putStrLn $ show b
  putStrLn "duplexClose finished"

-- | A two-way connection. Closes if receiver or sender sides close.
duplex' ::
  (WebSocketsData a) =>
  Box IO a a ->
  Connection ->
  IO ()
duplex' (Box c e) conn = do
  ((), closed) <-
    concurrently
      (receiver c conn)
      (sender e conn)
  when (not closed) $ sendClose conn ("duplex'" :: Text)
  -- FIXME: also wait for return close

-- | A two-way connection. Closes if receiver or sender sides close.
duplex'_ ::
  (Show a, WebSocketsData a) =>
  Box IO a a ->
  Committer IO (String, ConnectionException) ->
  Connection ->
  IO ()
duplex'_ (Box c e) cExceptions conn = do
  (recclose, sendclose) <-
    concurrently
      (receiver_ c (contramap ("receiver duplex'_:",) cExceptions) conn)
      (sender_ e (contramap ("sender duplex'_:",) cExceptions) conn)
  when (not (UnClosed == sendclose || recclose)) $ sendClose conn ("duplex'" :: Text)
  -- FIXME: also wait for return close
  pure ()

-- | A one-way sender
simplex ::
  (Show a, WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO ()
simplex e conn = do
  duplexClose (Box mempty e) conn

-- | A two-way connection. Closes when both receiver and sender sides close.
duplex_ ::
  (WebSocketsData a , Show a) =>
  Box IO a a ->
  Committer IO (String, ConnectionException) ->
  Connection ->
  IO ()
duplex_ (Box c e) cErrs conn = do
  _ <- concurrently (receiver_ c (contramap ("receiver duplex:",) cErrs) conn) (sender_ e (contramap ("sender duplex:",) cErrs) conn)
  pure ()

duplexClient ::
  (WebSocketsData a, Show a) =>
  SocketConfig ->
  Box IO a a ->
  IO ()
duplexClient cfg b = clientApp cfg (duplexClose b)

duplexClient_ ::
  (WebSocketsData a, Show a) =>
  SocketConfig ->
  Box IO a a ->
  Committer IO (String, ConnectionException) ->
  IO ()
duplexClient_ cfg b cErr = clientApp cfg (duplex_ b (contramap (first (<> "duplexClient_:")) cErr))

-- | Serve a connection action
serve ::
  ClientApp a -> ServerApp
serve action p =
  void $
    process
      action (connect p)

duplexServer ::
  (WebSocketsData a, Show a) =>
  SocketConfig ->
  Box IO a a ->
  IO ()
duplexServer cfg b = serverApp cfg (serve (duplexClose b))

duplexServer_ ::
  (WebSocketsData a, Show a) =>
  SocketConfig ->
  Box IO a a ->
  Committer IO (String, ConnectionException) ->
  IO ()
duplexServer_ cfg b cErr = serverApp cfg (serve (duplex_ b (contramap (first (<> "duplexServer_:")) cErr)))

-- | A receiver that responds based on received Text.
-- lefts are quit signals. Rights are response text.
responderF ::
  (WebSocketsData a) =>
  (a -> Either Text Text) ->
  Connection ->
  IO ()
responderF f conn =  go
  where
    go = do
      response <- try (receiveData conn)
      case response of
        Left (CloseRequest _ _) -> pure ()
        Left err -> throwIO err
        Right msg -> case f msg of
          Left reason ->
            sendClose conn reason >> go
          (Right msg') -> do
            confirm <- try (sendTextData conn msg')
            case confirm of
              Left (CloseRequest _ _) -> pure ()
              Left err -> throwIO err
              Right () -> go

-- | A receiver that responds based on received Text.
responderF_ ::
  (WebSocketsData a) =>
  (a -> Either Text Text) ->
  Committer IO (String, ConnectionException) ->
  ClientApp Bool
responderF_ f cExceptions conn = go
  where
    go = do
      response <- try (receiveData conn)
      case response of
        Left cr@(CloseRequest _ _) -> commit cExceptions ("responseF|receive",cr) >> pure True
        Left err -> commit cExceptions ("responseF|receive",err) >> pure False
        Right msg -> case f msg of
          Left reason ->
            sendClose conn reason >> go
          (Right msg') -> do
            confirm <- try (sendTextData conn msg')
            case confirm of
              Left cr@(CloseRequest _ _) -> commit cExceptions ("responseF|send",cr) >> pure True
              Left err -> commit cExceptions ("responseF|send",err) >> pure False
              Right () -> go

