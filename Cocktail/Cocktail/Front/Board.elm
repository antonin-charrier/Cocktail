module Board exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Tuple
import Piece
import Set


type alias Model =
    { focusOn : FocusOn
    , pieces : List Piece.Piece
    , possibleMoves : List (Int, Int)
    , capturedPiecesWhite : List Piece.Piece
    , capturedPiecesBlack : List Piece.Piece
    , state : State
    }

type Case = Full Piece.Piece
    | Empty

type FocusOn = FocusedPiece Piece.Piece
    | NoPiece (Int, Int)
    | PieceToCapture Piece.Piece

type State = Turn Player
    | Win Player

type Player = Player1
    | Player2

initialModel : Model
initialModel =
    { 
        focusOn = NoPiece ( 0, 0 )
        , pieces = [
            { coordinates = (1, 1), pieceType = Piece.Rook, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (2, 1), pieceType = Piece.Knight, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (3, 1), pieceType = Piece.Bishop, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (4, 1), pieceType = Piece.Queen, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (5, 1), pieceType = Piece.King, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (6, 1), pieceType = Piece.Bishop, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (7, 1), pieceType = Piece.Knight, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (8, 1), pieceType = Piece.Rook, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (1, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (2, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (3, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (4, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (5, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (6, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (7, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (8, 2), pieceType = Piece.Pawn, color = Piece.White, hasAlreadyMoved = False },
            { coordinates = (1, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (2, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (3, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (4, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (5, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (6, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (7, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (8, 7), pieceType = Piece.Pawn, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (1, 8), pieceType = Piece.Rook, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (2, 8), pieceType = Piece.Knight, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (3, 8), pieceType = Piece.Bishop, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (4, 8), pieceType = Piece.Queen, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (5, 8), pieceType = Piece.King, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (6, 8), pieceType = Piece.Bishop, color = Piece.Black , hasAlreadyMoved = False},
            { coordinates = (7, 8), pieceType = Piece.Knight, color = Piece.Black, hasAlreadyMoved = False },
            { coordinates = (8, 8), pieceType = Piece.Rook, color = Piece.Black, hasAlreadyMoved = False }
        ]
        , possibleMoves = []
        , capturedPiecesBlack = []
        , capturedPiecesWhite = []
        , state = Turn Player1
    }


type Msg
    = ClickOnCase FocusOn


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickOnCase focusOn ->
           case focusOn of
                FocusedPiece piece ->
                    { focusOn = focusOn, pieces = model.pieces, possibleMoves = ( Piece.possibleRealMoves piece model.pieces )
                        , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = model.state }
                NoPiece coordinates -> 
                    ( 
                        case model.focusOn of
                            FocusedPiece piece ->
                                { focusOn = NoPiece ( 0, 0 ), pieces = ( movePiece model.pieces piece coordinates ), possibleMoves = []
                                    , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = (
                                        case model.state of
                                            Turn player ->
                                                case player of
                                                    Player1 -> Turn Player2
                                                    Player2 -> Turn Player1
                                            Win player -> Win player
                                    ) }
                            NoPiece _ -> 
                                { focusOn = NoPiece ( 0, 0 ), pieces = model.pieces, possibleMoves = []
                                    , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = model.state }
                            PieceToCapture _ -> 
                                { focusOn = NoPiece ( 0, 0 ), pieces = model.pieces, possibleMoves = []
                                    , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = model.state }
                    )
                PieceToCapture pieceToCapture ->
                    case model.focusOn of
                        FocusedPiece piece ->
                            { focusOn = NoPiece ( 0, 0 ), pieces = ( moveAndCapture model.pieces piece pieceToCapture ), possibleMoves = []
                                , capturedPiecesBlack = (
                                    case pieceToCapture.color of
                                        Piece.Black ->
                                            pieceToCapture :: model.capturedPiecesBlack
                                        Piece.White -> 
                                            model.capturedPiecesBlack
                                ), capturedPiecesWhite = (
                                    case pieceToCapture.color of
                                        Piece.Black ->
                                            model.capturedPiecesWhite
                                        Piece.White -> 
                                            pieceToCapture :: model.capturedPiecesWhite
                                ), state = (
                                    if pieceToCapture.pieceType == Piece.King then
                                        case pieceToCapture.color of
                                            Piece.White -> Win Player2
                                            Piece.Black -> Win Player1
                                    else
                                        case model.state of
                                            Turn player ->
                                                case player of
                                                    Player1 -> Turn Player2
                                                    Player2 -> Turn Player1
                                            Win player -> Win player
                                ) }
                        NoPiece _ -> 
                            { focusOn = NoPiece ( 0, 0 ), pieces = model.pieces, possibleMoves = []
                                , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = model.state }
                        PieceToCapture _ -> 
                            { focusOn = NoPiece ( 0, 0 ), pieces = model.pieces, possibleMoves = []
                                , capturedPiecesBlack = model.capturedPiecesBlack, capturedPiecesWhite = model.capturedPiecesWhite, state = model.state }

movePiece : List Piece.Piece -> Piece.Piece -> (Int, Int) -> List Piece.Piece
movePiece pieces currentPiece newCoordinates = 
    { currentPiece | coordinates = newCoordinates, hasAlreadyMoved = True } :: List.filter ( \piece -> piece.coordinates /= currentPiece.coordinates ) pieces

capturePiece : List Piece.Piece -> Piece.Piece -> List Piece.Piece
capturePiece pieces pieceToCapture =
    List.filter ( \piece -> piece.coordinates /= pieceToCapture.coordinates ) pieces

moveAndCapture : List Piece.Piece -> Piece.Piece -> Piece.Piece -> List Piece.Piece
moveAndCapture pieces currentPiece pieceToCapture = 
    movePiece ( capturePiece pieces pieceToCapture ) currentPiece pieceToCapture.coordinates

view : Model -> Html Msg
view model =
    div [ class "board-container"] [ 
        div [ class "board" ] (buildBoard model 8)
        , div [ class "side-panel" ] [
            div [ class "captured-pieces-white" ] (buildCapturedPieces model.capturedPiecesWhite)
            , div [ class "announce" ] [ (buildAnnounce model.state) ]
            , div [ class "captured-pieces-black" ] (buildCapturedPieces model.capturedPiecesBlack)
        ]
    ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

buildBoard : Model -> Int -> List (Html Msg)
buildBoard model max =
    if max > 0 then
        List.append
            [ div [ class "line" ] (buildLine model max 8)
            ]
            (buildBoard model (max - 1))
    else
        []

buildAnnounce : State -> Html Msg
buildAnnounce state =
    case state of 
        Turn player ->
            case player of
                Player1 -> text "Player 1 (White), it is your turn to play"
                Player2 -> text "Player 2 (Black), it is your turn to play"
        Win player ->
            case player of
                Player1 -> text "Player 1 (White) wins!"
                Player2 -> text "Player 2 (Black) wins!"

buildCapturedPieces : List Piece.Piece -> List (Html Msg)
buildCapturedPieces capturedPieces = 
    List.map ( \piece -> div [] [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ] ) capturedPieces

buildLine : Model -> Int -> Int -> List (Html Msg)
buildLine model id max =
    if max > 0 then
        List.append [ buildCase model max id ] (buildLine model id (max - 1))

    else
        []

buildCase : Model -> Int -> Int -> Html Msg
buildCase model posX posY =
    div [ class "case" ]
        (
            case (findCaseFromCoordinates (posX, posY) model.pieces) of
                Empty ->
                    (
                        case (List.member (posX, posY) model.possibleMoves) of
                            False ->
                                [ button [ onClick ( ClickOnCase ( NoPiece ( posX, posY ) ) ), disabled True ] [] ]
                            True -> 
                                case model.state of
                                    Turn player ->
                                        [ button [ onClick ( ClickOnCase ( NoPiece ( posX, posY ) ) ), class "possible-move" ] [] ]
                                    Win _ ->
                                        [ button [ onClick ( ClickOnCase ( NoPiece ( posX, posY ) ) ), disabled True ] [] ]
                    )
                Full piece ->
                    (
                        case (List.member (posX, posY) model.possibleMoves) of
                            False ->
                                case model.state of
                                    Turn player ->
                                        if player == Player1 && piece.color == Piece.White then
                                            [ button [ onClick ( ClickOnCase ( FocusedPiece piece ) ) ]
                                                [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                            ]
                                        else if player == Player2 && piece.color == Piece.Black then
                                            [ button [ onClick ( ClickOnCase ( FocusedPiece piece ) ) ]
                                                [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                            ]
                                        else
                                            [ button [ onClick ( ClickOnCase ( FocusedPiece piece ) ), disabled True ]
                                                [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                            ]
                                    Win _ ->
                                        [ button [ onClick ( ClickOnCase ( FocusedPiece piece ) ), disabled True ]
                                            [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                        ]

                            True ->
                                case model.state of
                                    Turn player ->
                                        [ button [ onClick ( ClickOnCase ( PieceToCapture piece ) ), class "possible-move" ]
                                            [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                        ]
                                    Win _ ->
                                        [ button [ onClick ( ClickOnCase ( FocusedPiece piece ) ), disabled True ]
                                            [ img [ src ("src/" ++ (srcImageFromPiece piece)) ] [] ]
                                        ]
                    )
                        
        )

findCaseFromCoordinates : (Int, Int) -> List Piece.Piece -> Case
findCaseFromCoordinates =
    \pos -> \pieces ->
        case (List.head (filterFromCoordinates pos pieces)) of
            Just piece -> Full piece
            _ -> Empty


filterFromCoordinates = \pos -> \pieces ->
    List.filter (\item -> ((Tuple.first item.coordinates) == (Tuple.first pos) && (Tuple.second item.coordinates) == (Tuple.second pos))) pieces
    
srcImageFromPiece : Piece.Piece -> String
srcImageFromPiece = \piece ->        
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
    