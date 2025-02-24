port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Port to send data to JavaScript
port sendMessage : String -> Cmd msg

-- Port to receive data from JavaScript
port receiveMessage : (String -> msg) -> Sub msg

type Msg
  = Increment
  | Decrement
  | ReceiveMessage String

type alias Model =
  { count : Int
  , message : String
  }

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  ({ count = 0, message = "" }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({ model | count = model.count + 1 }, sendMessage "Incremented!")

    Decrement ->
      ({ model | count = model.count - 1 }, sendMessage "Decremented!")

    ReceiveMessage message ->
      ({ model | message = message }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  receiveMessage ReceiveMessage

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ text model.message ]
    ]