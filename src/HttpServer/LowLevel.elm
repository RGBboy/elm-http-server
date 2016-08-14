module HttpServer.LowLevel exposing
  ( listen
  , Settings
  , reply
  , close
  , Server
  , Request
  )

import Native.HttpServer
import Task exposing (Task)

type Server = Server

type Request = Request

{-| Attempt to listen to a particular port.
-}
listen : Int -> Settings -> Task x Server
listen portNumber settings =
  let
    test = Debug.log "TEST" portNumber
  in
    Native.HttpServer.listen portNumber settings

{-|
-}
type alias Settings =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

{-|
-}
reply : Request -> String -> Task x ()
reply =
  Native.HttpServer.reply

{-|
-}
close : Server -> Task x ()
close =
  Native.HttpServer.close
