module Board exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Tuple
import Piece


type alias Model =
    { focusOn : FocusOn
    , pieces : List Piece.Piece
    , possibleMoves : List (Int, Int)
    }

type Case = Full Piece.Piece
    | Empty

type FocusOn = FocusedPiece Piece.Piece
    | NoPiece


initialModel : Model
initialModel =
    { focusOn = NoPiece, pieces = [
            { coordinates = (1, 1), pieceType = Piece.Rook, color = Piece.White },
            { coordinates = (2, 1), pieceType = Piece.Knight, color = Piece.White },
            { coordinates = (3, 1), pieceType = Piece.Bishop, color = Piece.White },
            { coordinates = (4, 1), pieceType = Piece.Queen, color = Piece.White },
            { coordinates = (5, 1), pieceType = Piece.King, color = Piece.White },
            { coordinates = (6, 1), pieceType = Piece.Bishop, color = Piece.White },
            { coordinates = (7, 1), pieceType = Piece.Knight, color = Piece.White },
            { coordinates = (8, 1), pieceType = Piece.Rook, color = Piece.White },
            { coordinates = (1, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (2, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (3, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (4, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (5, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (6, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (7, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (8, 2), pieceType = Piece.Pawn, color = Piece.White },
            { coordinates = (1, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (2, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (3, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (4, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (5, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (6, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (7, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (8, 7), pieceType = Piece.Pawn, color = Piece.Black },
            { coordinates = (1, 8), pieceType = Piece.Rook, color = Piece.Black },
            { coordinates = (2, 8), pieceType = Piece.Knight, color = Piece.Black },
            { coordinates = (3, 8), pieceType = Piece.Bishop, color = Piece.Black },
            { coordinates = (4, 8), pieceType = Piece.Queen, color = Piece.Black },
            { coordinates = (5, 8), pieceType = Piece.King, color = Piece.Black },
            { coordinates = (6, 8), pieceType = Piece.Bishop, color = Piece.Black },
            { coordinates = (7, 8), pieceType = Piece.Knight, color = Piece.Black },
            { coordinates = (8, 8), pieceType = Piece.Rook, color = Piece.Black }
        ], possibleMoves = []
    }


type Msg
    = ClickOnCase Piece.Piece


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickOnCase piece ->
            { focusOn = piece, pieces = model.pieces, possibleMoves = (Piece.possibleRegularMoves piece) }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "board" ] (buildBoard model 8) ]


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
                (
                    case (findCaseFromCoordinates (posX, posY) model.pieces) of
                        Empty ->
                            (
                                case (List.member (posX, posY) model.possibleMoves) of
                                    True ->
                                        [ button [ onClick (ClickOnCase model.focusOn), disabled True ] [] ]
                                    False -> 
                                        [ button [ onClick (ClickOnCase model.focusOn), class "possible-move" ] [] ]                                    
                            )
                        Full caseWithImg -> 
                            [ button [ onClick (ClickOnCase model.focusOn) ]
                                [ img [ src ("src/" ++ (caseToImage caseWithImg)) ] [] ]
                            ]                            
                )

findCaseFromCoordinates : (Int, Int) -> List Piece.Piece -> Case
findCaseFromCoordinates =
    \pos -> \pieces ->
        case (List.head (filterFromCoordinates pos pieces)) of
            Just piece -> Full piece
            _ -> Empty


filterFromCoordinates = \pos -> \pieces ->
    List.filter (\item -> ((Tuple.first item.coordinates) == (Tuple.first pos) && (Tuple.second item.coordinates) == (Tuple.second pos))) pieces
    
caseToImage : Piece.Piece -> String
caseToImage = \piece ->        
    case piece.pieceType of
        Piece.King ->
            case piece.color of
                Piece.White ->
                    "icon.king.light.svg"
                Piece.Black ->
                    "icon.king.dark.svg"
        Piece.Queen ->
            case piece.color of
                Piece.White ->
                    "icon.queen.light.svg"
                Piece.Black ->
                    "icon.queen.dark.svg"
        Piece.Bishop ->
            case piece.color of
                Piece.White ->
                    "icon.bishop.light.svg"
                Piece.Black ->
                    "icon.bishop.dark.svg"
        Piece.Knight ->
            case piece.color of
                Piece.White ->
                    "icon.knight.light.svg"
                Piece.Black ->
                    "icon.knight.dark.svg"
        Piece.Rook ->
            case piece.color of
                Piece.White ->
                    "icon.rook.light.svg"
                Piece.Black ->
                    "icon.rook.dark.svg"
        Piece.Pawn ->
            case piece.color of
                Piece.White ->
                    "icon.pawn.light.svg"
                Piece.Black ->
                    "icon.pawn.dark.svg"
    