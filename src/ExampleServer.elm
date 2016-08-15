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


update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    Request request ->
      let
        newModel = model + 1
      in
        ( Debug.log "Request" newModel
        , HttpServer.reply request ("Hello request " ++ toString(newModel))
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
