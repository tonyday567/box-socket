box-socket
===

[![Build Status](https://travis-ci.org/tonyday567/box-socket.svg)](https://travis-ci.org/tonyday567/box-socket) [![Hackage](https://img.shields.io/hackage/v/box-socket.svg)](https://hackage.haskell.org/package/box-socket) [![lts](https://www.stackage.org/package/box-socket/badge/lts)](http://stackage.org/lts/package/box-socket) [![nightly](https://www.stackage.org/package/box-socket/badge/nightly)](http://stackage.org/nightly/package/box-socket) 

flow
----

```
stack build --exec "$(stack path --local-install-root)/bin/box-socket" --file-watch
```

Should produce:

```
[Left "receiver: received: echo:1",Right "echo:1",Left "receiver: received: echo:2",Right "echo:2",Left "receiver: received: echo:3",Right "echo:3",Left "receiver: received: close: 1000 \"received close signal: responder closed.\""]
```
