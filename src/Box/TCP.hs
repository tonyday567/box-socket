{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Box.TCP
  ( TCPConfig(..),
    defaultTCPConfig,
    Env(..),
    defaultVersion,
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
  ) where

import Box
import NumHask.Prelude hiding (handle, check)
import Network.Simple.TCP
import Control.Lens

data TCPConfig
  = TCPConfig
      { host :: Text,
        port :: Text
      }
  deriving (Show, Eq, Generic)

defaultTCPConfig :: TCPConfig
defaultTCPConfig = TCPConfig "127.0.0.1" "3566"
-- ib 7496

-- | an active
data Env
  = Env
      { socket :: Socket,
        sockaddr :: SockAddr,
        ascreendump :: Maybe (Async ()),
        afiledump :: Maybe (Async ())
      }

defaultVersion :: Text
defaultVersion = "11"

-- | Connects to a server
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

tcpEmitter :: Socket -> Emitter IO ByteString
tcpEmitter s = Emitter $ recv s 2048

tcpCommitter :: Socket -> Committer IO ByteString
tcpCommitter s = Committer $ \bs -> send s bs *> pure True

tcpBox :: Socket -> Box IO ByteString ByteString
tcpBox s = Box (tcpCommitter s) (tcpEmitter s)

tcpServer :: TCPConfig -> Box IO ByteString ByteString -> IO ()
tcpServer cfg (Box c e) =
  serve HostAny (unpack $ port cfg)
      (\(s,_) -> void $ do
          race
            (glue (tcpCommitter s) e)
            (glue c (tcpEmitter s)))

responder :: (ByteString -> IO ByteString) -> Box IO ByteString ByteString -> IO ()
responder f (Box c e) =
  glue c (mapE (\bs -> Just <$> f bs) e)

--  A server that explicitly responds to client messages.
tcpResponder :: TCPConfig -> (ByteString -> IO ByteString) -> IO ()
tcpResponder cfg f =
  serve HostAny (unpack $ port cfg)
    (\(s,_) -> responder f (Box (tcpCommitter s) (tcpEmitter s)))

tcpSender :: TCPConfig -> Emitter IO ByteString -> IO ()
tcpSender cfg e =
  serve HostAny (unpack $ port cfg)
    (\(s,_) -> glue (tcpCommitter s) e)

tcpStdClient :: TCPConfig -> IO ()
tcpStdClient cfg = do
  (Env s _ _ _) <- new cfg
  void $ concurrently
     (glue o (tcpEmitter s))
     (glue (tcpCommitter s) i)
  where
    o = contramap decodeUtf8 toStdout
    i = fmap encodeUtf8 fromStdin

testHarness :: IO () -> IO ()
testHarness io =
  void $ race
    io
    (cancelQ fromStdin)

cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    Just x -> putStrLn ("badly handled: " <> x)
    Nothing -> pure ()

testResponder :: IO ()
testResponder = testHarness (tcpResponder defaultTCPConfig (pure . ("echo: " <>)))

testServerSender :: IO ()
testServerSender = testHarness $
  tcpSender defaultTCPConfig <$.> (fromListE ["hi!"])




