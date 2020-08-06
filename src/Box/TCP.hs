{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Box.TCP where

import Box
import NumHask.Prelude hiding (handle)
import qualified Network.Socket as S
import System.IO (hClose, BufferMode(..), hSetBuffering)
import GHC.IO.Handle (hFlushAll)
import System.IO (hIsOpen)
import qualified Prelude as P

data TCPConfig
  = TCPConfig
      { host :: Text,
        port :: Int,
        clientId :: Int,
        extraAuth :: Bool
      }
  deriving (Show, Eq, Generic)

defaultTCPConfig :: TCPConfig
defaultTCPConfig = TCPConfig "127.0.0.1" 7496 0 False

-- | an active
data Env
  = Env
      { handle :: Handle,
        ascreendump :: Maybe (Async ()),
        afiledump :: Maybe (Async ()),
        box :: Box IO Text Text
      }

defaultVersion :: Text
defaultVersion = "11"

lineBox :: Handle -> Box IO Text Text
lineBox h = Box (handleC h) (handleE h)

-- | Connects to a server
new ::
  -- | Configuration
  TCPConfig ->
  (Handle -> IO ()) ->
  IO Env
new cfg hio = do
  addrinfos <- S.getAddrInfo Nothing (Just $ unpack $ host cfg) (Just $ show (port cfg))
  let serveraddr = P.head addrinfos
  s <- S.socket (S.addrFamily serveraddr) S.Stream S.defaultProtocol
  S.connect s (S.addrAddress serveraddr)
  h <- S.socketToHandle s ReadWriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hio h
  pure (Env h Nothing Nothing (lineBox h))

-- | close an Env
close :: Env -> IO ()
close e = do
  hClose (handle e)
  maybe (pure ()) cancel (ascreendump e)
  maybe (pure ()) cancel (afiledump e)

-- | check an Env
check :: Env -> IO Text
check e = do
  hFlushAll (handle e)
  o <- hIsOpen (handle e)
  pure $ ("check: " :: Text) <> bool "closed" "open" o

