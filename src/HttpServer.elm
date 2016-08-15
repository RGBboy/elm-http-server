effect module HttpServer where { command = MyCmd, subscription = MySub } exposing
  ( reply
  , listen
  , Request
  )

{-|

-}

import Dict
import Process
import Task exposing (Task)
import HttpServer.LowLevel as Http

-- type alias Server = Http.Server

type alias Request = Http.Request

-- type alias Request =
--   { process : Process.Id
--   , headers : List (String, String)
--   , body : Body
--   }

-- type alias Response =
--   { status : Int
--   , statusText : String
--   , headers : Dict String String
--   , value : Value
--   }

-- COMMANDS


type MyCmd msg
  = Reply Request String


{-| Reply to a particular request. You might say something like this:

    reply request "Hello!"
-}
reply : Request -> String -> Cmd msg
reply request message =
  command (Reply request message)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Reply request msg) =
  Reply request msg



-- SUBSCRIPTIONS


type MySub msg
  = Listen Int (Request -> msg)


{-| Subscribe to any incoming request to the server.
-}
listen : Int -> (Request -> msg) -> Sub msg
listen portNumber tagger =
  subscription (Listen portNumber tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Listen portNumber tagger ->
      Listen portNumber (tagger >> func)



-- MANAGER


type alias State msg =
  { servers : ServersDict
  , subs : SubsDict msg
  }


type alias ServersDict =
  Dict.Dict Int Server


type alias SubsDict msg =
  Dict.Dict Int (List (Request -> msg))


type Server
  = Opening Process.Id
  | Listening Http.Server


init : Task Never (State msg)
init =
  Task.succeed (State Dict.empty Dict.empty)



-- HANDLE APP MESSAGES


(&>) t1 t2 = Task.andThen t1 (\_ -> t2)


onEffects
  : Platform.Router msg Msg
  -> List (MyCmd msg)
  -> List (MySub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router cmds subs state =
  let
    -- things to listen to if not already
    newSubs =
      buildSubDict subs Dict.empty

    cleanup _ =
      let
        newEntries = (Dict.map (\k v -> []) newSubs)

        leftStep portNumber _ getNewServers =
          getNewServers
            `Task.andThen` \newServers ->

          attemptOpen router portNumber
            `Task.andThen` \pid ->

          Task.succeed (Dict.insert portNumber (Opening pid) newServers)

        bothStep portNumber _ server getNewServers =
          Task.map (Dict.insert portNumber server) getNewServers

        rightStep portNumber server getNewServers =
          close server &> getNewServers
      in
        Dict.merge leftStep bothStep rightStep newEntries state.servers (Task.succeed Dict.empty)
          `Task.andThen` \newServers ->

        Task.succeed (State newServers newSubs)

  in
    sendReplies cmds `Task.andThen` cleanup




sendReplies : List (MyCmd msg) -> Task x ()
sendReplies cmds =
  case cmds of
    [] ->
      Task.succeed ()

    Reply request msg :: rest ->
      Http.reply request msg
        &> sendReplies rest


buildSubDict : List (MySub msg) -> SubsDict msg -> SubsDict msg
buildSubDict subs dict =
  case subs of
    [] ->
      dict

    Listen portNumber tagger :: rest ->
      buildSubDict rest (Dict.update portNumber (add tagger) dict)

add : a -> Maybe (List a) -> Maybe (List a)
add value maybeList =
  case maybeList of
    Nothing ->
      Just [value]

    Just list ->
      Just (value :: list)



-- HANDLE SELF MESSAGES


type Msg
  = Request Int Request
  | Die Int
  | Open Int Http.Server


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    Request portNumber request ->
      let
        requests =
          Dict.get portNumber state.subs
            |> Maybe.withDefault []
            |> List.map (\tagger -> Platform.sendToApp router (tagger request))
      in
        Task.sequence requests &> Task.succeed state

    Die portNumber ->
      case Dict.get portNumber state.servers of
        Nothing ->
          Task.succeed state

        Just _ ->
          attemptOpen router portNumber
            `Task.andThen` \pid ->

          Task.succeed (updateServer portNumber (Opening pid) state)

    Open portNumber server ->
      Task.succeed (updateServer portNumber (Listening server) state)


removeServer : Int -> State msg -> State msg
removeServer portNumber state =
  { state | servers = Dict.remove portNumber state.servers }


updateServer : Int -> Server -> State msg -> State msg
updateServer portNumber server state =
  { state | servers = Dict.insert portNumber server state.servers }


attemptOpen : Platform.Router msg Msg -> Int -> Task x Process.Id
attemptOpen router portNumber =
  let
    actuallyAttemptOpen =
      open router portNumber
        `Task.andThen` \server ->
      Platform.sendToSelf router (Open portNumber server)
  in
    Process.spawn actuallyAttemptOpen


open : Platform.Router msg Msg -> Int -> Task x Http.Server
open router portNumber =
  Http.listen portNumber
    { onRequest = \request -> Platform.sendToSelf router (Request portNumber request)
    , onClose = \_ -> Platform.sendToSelf router (Die portNumber)
    }


-- CLOSE SERVER


close : Server -> Task x ()
close server =
  case server of
    Opening pid ->
      Process.kill pid

    Listening server ->
      Http.close server
