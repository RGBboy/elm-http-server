import Html exposing (Html)
import Html.App as App
import Task exposing (Task, andThen)
import Process

import HttpServer

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- hack for server program to work
view : Model -> Html Msg
view = always (Html.text "")

-- Model

type alias Model = Int


init : (Model, Cmd Msg)
init =
  ( 0
  , Cmd.none
  )

-- Messages

type Msg
  = Noop
  | Request HttpServer.Request
  | Echo String


update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    Echo message ->
      ( Debug.log "Request" (model + 1)
      , Cmd.none
      )
    Request request ->
      ( Debug.log "Request" (model + 1)
      , HttpServer.reply request ("Hello request " ++ toString(model))
      )
    _ ->
      ( model
      , Cmd.none
      )


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  if (model < 5) then
    HttpServer.listen 8080 Request
  else
    Sub.none
