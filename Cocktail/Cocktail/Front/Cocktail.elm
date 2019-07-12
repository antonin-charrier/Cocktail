module Cocktail exposing (..)

import Browser
import Html exposing (Html, button, div, text, pre)
import Html.Events exposing (onClick)
import Http

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Coordonate = { x : Int, y : Int }
type alias Move = { from : Coordonate, to : Coordonate }

type Model
  = Failure
  | Loading
  | Success String
  | Nothing

init : () -> (Model, Cmd Msg)
init _ =
  ( Nothing
  , Cmd.none
  )

-- UPDATE

type Msg
  = GotText (Result Http.Error String)
  | OnMove Move
  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)
    OnMove move -> onMove move

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div [] [button [ onClick (OnMove {from = { x = 0, y = 0}, to = { x = 1, y = 1}})] [ text "test"]]

  -- FUNCTIONS

onMove move =
    ( Loading
    , Http.post
      { url = "http://localhost:8080/move"
      , body = (Http.stringBody "applicaion/json" (createJsonFromMove move))
      , expect = Http.expectString GotText
      }
    )

createJsonFromMove : Move -> String
createJsonFromMove move =
    "{\"from\":[" ++ (String.fromInt move.from.x)  ++ "," ++ (String.fromInt move.from.y) ++ "],"
    ++ "\"to\":[" ++ (String.fromInt move.to.x)  ++ "," ++ (String.fromInt move.to.y) ++ "]}"
    
    