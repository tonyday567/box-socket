{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | TCP Boxes.
module Box.TCP
  ( TCPConfig (..),
    defaultTCPConfig,
    Env (..),
    new,
    close,
    tcpEmitter,
    tcpCommitter,
    tcpBox,
    tcpServer,
    tcpResponder,
    tcpSender,
    tcpStdClient,
    testHarness,
    testResponder,
    testServerSender,
  )
where

import Box
import Control.Lens
import Network.Simple.TCP
import NumHask.Prelude hiding (check, handle)

-- | TCP configuration
--
-- >>> defaultTCPConfig
-- TCPConfig {host = "127.0.0.1", port = "3566"}
data TCPConfig = TCPConfig
  { host :: Text,
    port :: Text
  }
  deriving (Show, Eq, Generic)

-- | default
defaultTCPConfig :: TCPConfig
defaultTCPConfig = TCPConfig "127.0.0.1" "3566"

-- | An active TCP environment
data Env = Env
  { socket :: Socket,
    sockaddr :: SockAddr,
    -- | A screen dump thread
    ascreendump :: Maybe (Async ()),
    -- | A file dump thread
    afiledump :: Maybe (Async ())
  }

-- | Connects to a server with no screen or file dump.
new ::
  -- | Configuration
  TCPConfig ->
  IO Env
new cfg = do
  (sock, sa) <- connectSock (unpack $ host cfg) (unpack $ port cfg)
  pure (Env sock sa Nothing Nothing)

-- | close an Env
close :: Env -> IO ()
close env = do
  closeSock (socket env)
  maybe (pure ()) cancel (ascreendump env)
  maybe (pure ()) cancel (afiledump env)

-- | Emits from a 'Socket'
tcpEmitter :: Socket -> Emitter IO ByteString
tcpEmitter s = Emitter $ recv s 2048

-- | Commits to a 'Socket'
tcpCommitter :: Socket -> Committer IO ByteString
tcpCommitter s = Committer $ \bs -> send s bs $> True

-- | 'Box' connection for a 'Socket'
tcpBox :: Socket -> Box IO ByteString ByteString
tcpBox s = Box (tcpCommitter s) (tcpEmitter s)

-- | TCP server 'Box'
tcpServer :: TCPConfig -> Box IO ByteString ByteString -> IO ()
tcpServer cfg (Box c e) =
  serve
    HostAny
    (unpack $ port cfg)
    ( \(s, _) ->
        void $
          race
            (glue (tcpCommitter s) e)
            (glue c (tcpEmitter s))
    )

-- | Response function.
responder :: (ByteString -> IO ByteString) -> Box IO ByteString ByteString -> IO ()
responder f (Box c e) =
  glue c (mapE (fmap Just . f) e)

-- | A server that explicitly responds to client messages.
tcpResponder :: TCPConfig -> (ByteString -> IO ByteString) -> IO ()
tcpResponder cfg f =
  serve
    HostAny
    (unpack $ port cfg)
    (\(s, _) -> responder f (Box (tcpCommitter s) (tcpEmitter s)))

-- | A server independent of incoming messages.
tcpSender :: TCPConfig -> Emitter IO ByteString -> IO ()
tcpSender cfg e =
  serve
    HostAny
    (unpack $ port cfg)
    (\(s, _) -> glue (tcpCommitter s) e)

-- | A TCP client connected to stdin
tcpStdClient :: TCPConfig -> IO ()
tcpStdClient cfg = do
  (Env s _ _ _) <- new cfg
  void $
    concurrently
      (glue o (tcpEmitter s))
      (glue (tcpCommitter s) i)
  where
    o = contramap decodeUtf8 toStdout
    i = fmap encodeUtf8 fromStdin

-- | test harness wrapping an action with a "q" escape.
testHarness :: IO () -> IO ()
testHarness io =
  void $
    race
      io
      (cancelQ fromStdin)

-- | Cancel with a "q".
cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    Just x -> putStrLn ("badly handled: " <> x)
    Nothing -> pure ()

-- | @"echo: " <>@ Responder
testResponder :: IO ()
testResponder = testHarness (tcpResponder defaultTCPConfig (pure . ("echo: " <>)))

-- | Test server.
testServerSender :: IO ()
testServerSender =
  testHarness $
    tcpSender defaultTCPConfig <$.> fromListE ["hi!"]
