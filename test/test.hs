{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "app/box-socket.hs",
    "src/Box/Socket.hs",
    "src/Box/TCP.hs"
  ]
