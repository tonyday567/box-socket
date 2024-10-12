
# Table of Contents

1.  [Usage](#org6c95983)
2.  [Design](#org6cb8437)

[![img](https://img.shields.io/hackage/v/box-socket.svg)](https://hackage.haskell.org/package/box) [![img](https://github.com/tonyday567/box-socket/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/box/actions?query=workflow%3Ahaskell-ci)

Socket API based on the box library, with websockets and TCP support.


<a id="org6c95983"></a>

# Usage

    :set -XOverloadedStrings
    import Box
    import Box.Socket.Types
    import Box.Websocket

IO client:

    clientBox defaultSocketConfig (CloseAfter 0) (stdBox "q")

IO server:

    serverBox defaultSocketConfig (CloseAfter 0) (stdBox "q")

See examples in Box.Websocket.Example and Box.TCP.Example for a variety of usage.


<a id="org6cb8437"></a>

# Design

-   The API attempts to be similar for TCP and Websocket

-   A Codensity, continuation passing style is encouraged, similar to the box library.

