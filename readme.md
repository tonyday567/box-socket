box-socket
===
[![Hackage](https://img.shields.io/hackage/v/box-socket.svg)](https://hackage.haskell.org/package/box-socket)
[![Build Status](https://github.com/tonyday567/box-socket/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/box-socket/actions?query=workflow%3Ahaskell-ci) [![Hackage Deps](https://img.shields.io/hackage-deps/v/box-socket.svg)](http://packdeps.haskellers.com/reverse/box-socket)

flow
----

`testRun` in app/app.hs should produce:

```
[Left "receiver: received: echo:1",Right "echo:1",Left "receiver: received: echo:2",Right "echo:2",Left "receiver: received: echo:3",Right "echo:3",Left "receiver: received: close: 1000 \"received close signal: responder closed.\""]
```
