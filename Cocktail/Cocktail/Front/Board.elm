module Board exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, table, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String
import Tuple

type alias Token =
    { position : (Int, Int)
    , tokenType : String
    , color : String
    }


type alias Model =
    { focusOn : (Int, Int)
    , tokens : List Token
    }

type Case = Full Token
    | Empty


initialModel : Model
initialModel =
    { focusOn = (0,0), tokens = [{ position = (2,4), tokenType =  "Queen", color = "Black"},
            { position = (5,1), tokenType =  "Tower", color = "White"}
            ]
        }


type Msg
    = ClickOnCase (Int, Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickOnCase position ->
            { focusOn = position, tokens = model.tokens }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "board" ] (buildBoard model 8)
        , div [] [ text (String.fromInt (Tuple.first model.focusOn) ++ ":" ++ String.fromInt (Tuple.second model.focusOn) ++ " ==> ")
            , text (caseToString (findTokenFromPosition model.focusOn model.tokens))]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


buildBoard =
    \model ->
        \max ->
            if max > 0 then
                List.append
                    [ div [ class "line" ] (buildLine model max 8)
                    ]
                    (buildBoard model (max - 1))

            else
                []


buildLine = \model ->
    \id ->
        \max ->
            if max > 0 then
                List.append [ buildCase model max id ] (buildLine model id (max - 1))

            else
                []


buildCase = \model ->
    \posX ->
        \posY ->
            div [ class "case" ]
                [ button [ onClick (ClickOnCase (posX, posY)) ]
                    [ text (caseToString (findTokenFromPosition ( posX, posY ) model.tokens))
                    ]
                ]

findTokenFromPosition : (Int, Int) -> List Token -> Case
findTokenFromPosition =
    \pos -> \tokens ->
        case (List.head (filterFromPosition pos tokens)) of
            Just token  -> Full token
            _ -> Empty


filterFromPosition = \pos -> \tokens ->
    List.filter (\item -> ((Tuple.first item.position) == (Tuple.first pos) && (Tuple.second item.position) == (Tuple.second pos))) tokens

caseToString : Case -> String
caseToString = \c ->
    case c of
        Empty -> ""
        Full token -> token.color ++ " " ++ token.tokenType
    