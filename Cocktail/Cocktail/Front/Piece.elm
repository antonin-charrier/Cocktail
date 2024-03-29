module Piece exposing (Piece, PieceType(..), Color(..), Vector(..), possibleRealMoves, possibleRegularMoves)

import Set

type alias Piece =
    { coordinates : (Int, Int)
    , pieceType : PieceType
    , color : Color
    , hasAlreadyMoved : Bool
    }

type PieceType
    = King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn

type Color
    = White
    | Black

type Vector
    = Up
    | Down
    | Right
    | Left

possibleRealMoves : Piece -> List Piece -> List (Int, Int)
possibleRealMoves piece pieces = 
    filterPawnMoves (
        filterAllyObstacles (
            Set.toList ( possibleRegularMoves piece )
        ) piece pieces
    ) piece pieces

filterAllyObstacles : List (Int, Int) -> Piece -> List Piece -> List (Int, Int)
filterAllyObstacles moves piece pieces = 
    List.filter ( \coordinates ->
        List.all ( \otherPiece -> piece.color /= otherPiece.color || otherPiece.coordinates /= coordinates) pieces
    ) moves

filterPawnMoves : List (Int, Int) -> Piece -> List Piece -> List (Int, Int)
filterPawnMoves moves piece pieces = 
    List.filter ( \coordinates ->
        ( List.all ( \otherPiece ->
            ( piece.pieceType /= Pawn )
            || ( otherPiece.coordinates /= coordinates &&
                (
                    coordinates == ( ( Tuple.first piece.coordinates ), ( 
                        case piece.color of
                            Black ->Tuple.second piece.coordinates - 1
                            White -> Tuple.second piece.coordinates + 1
                        )
                    ) 
                    || coordinates == ( ( Tuple.first piece.coordinates ), (
                        case piece.color of
                            Black ->Tuple.second piece.coordinates - 2
                            White -> Tuple.second piece.coordinates + 2
                        )
                    )
                )
            )
        ) pieces ) 
        || ( List.any (\otherPiece ->
            otherPiece.color /= piece.color && otherPiece.coordinates == coordinates &&
            (
                coordinates == ( ( Tuple.first piece.coordinates - 1 ),  ( 
                    case piece.color of
                        Black ->Tuple.second piece.coordinates - 1
                        White -> Tuple.second piece.coordinates + 1
                    )
                )
                || coordinates == ( ( Tuple.first piece.coordinates + 1 ),  ( 
                    case piece.color of
                        Black ->Tuple.second piece.coordinates - 1
                        White -> Tuple.second piece.coordinates + 1
                    )
                )
            )
        ) pieces )
    ) moves

possibleRegularMoves : Piece -> Set.Set (Int, Int)
possibleRegularMoves piece =
    Set.fromList (
        case piece.pieceType of
            King ->            
                fullMoves (regularMoves [Up] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Down] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Right] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Left] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Up, Right] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Up, Left] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Down, Right] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Down, Left] piece.coordinates) 1 piece.coordinates
            Queen ->            
                fullMoves (regularMoves [Up] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Left] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Up, Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Up, Left] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down, Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down, Left] piece.coordinates) 8 piece.coordinates
            Bishop ->            
                fullMoves (regularMoves [Up, Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Up, Left] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down, Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down, Left] piece.coordinates) 8 piece.coordinates
            Knight ->            
                fullMoves (regularMoves [Up, Up, Right] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Up, Up, Left] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Down, Down, Right] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Down, Down, Left] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Right, Right, Up] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Right, Right, Down] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Left, Left, Up] piece.coordinates) 1 piece.coordinates
                ++ fullMoves (regularMoves [Left, Left, Down] piece.coordinates) 1 piece.coordinates
            Rook ->        
                fullMoves (regularMoves [Up] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Down] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Right] piece.coordinates) 8 piece.coordinates
                ++ fullMoves (regularMoves [Left] piece.coordinates) 8 piece.coordinates
            Pawn ->
                case piece.color of
                    White ->
                        fullMoves (regularMoves [Up] piece.coordinates) 1 piece.coordinates
                        ++ (
                            case piece.hasAlreadyMoved of
                                True -> []
                                False -> fullMoves (regularMoves [Up] piece.coordinates) 2 piece.coordinates
                        )
                        ++ fullMoves (regularMoves [Up, Right] piece.coordinates) 1 piece.coordinates
                        ++ fullMoves (regularMoves [Up, Left] piece.coordinates) 1 piece.coordinates
                    Black ->
                        fullMoves (regularMoves [Down] piece.coordinates) 1 piece.coordinates
                        ++ (
                            case piece.hasAlreadyMoved of
                                True -> []
                                False -> fullMoves (regularMoves [Down] piece.coordinates) 2 piece.coordinates
                        )
                        ++ fullMoves (regularMoves [Down, Right] piece.coordinates) 1 piece.coordinates
                        ++ fullMoves (regularMoves [Down, Left] piece.coordinates) 1 piece.coordinates
    )

fullMoves : (Int, Int) -> Int -> (Int, Int) -> List (Int, Int)
fullMoves moves multiplicator coordinates = 
   List.map (fullMoveWithCoordinates coordinates) (List.map ( regularMoveToFullMove moves ) ( List.range 1 multiplicator ) )

fullMoveWithCoordinates : (Int, Int) -> (Int, Int) -> (Int, Int)
fullMoveWithCoordinates coordinates vector = 
    ( ( Tuple.first coordinates ) + ( Tuple.first vector ), ( Tuple.second coordinates ) + ( Tuple.second vector ) )

regularMoveToFullMove : (Int, Int) -> Int -> (Int, Int)
regularMoveToFullMove coordinates multiplicator =
    ( ( Tuple.first coordinates ) * ( multiplicator - 1 ) + ( Tuple.first coordinates ), ( Tuple.second coordinates ) * ( multiplicator - 1 ) + ( Tuple.second coordinates ) )

regularMoves : List Vector -> (Int, Int) -> (Int, Int)
regularMoves vectors coordinates =
    List.foldl reduceRegularMoves (0, 0) (List.map (vectorToRegularMove coordinates) vectors)

reduceRegularMoves : (Int, Int) -> (Int, Int) -> (Int, Int)
reduceRegularMoves coordinatesA coordinatesB =
    ( ( Tuple.first coordinatesA ) + ( Tuple.first coordinatesB ), ( Tuple.second coordinatesA ) + ( Tuple.second coordinatesB ) )

vectorToRegularMove : (Int, Int) -> Vector -> (Int, Int)
vectorToRegularMove coordinates vector = 
    case vector of
        Up ->            
            ( 0, 1 )
        Down ->            
            ( 0, -1 )
        Right ->            
            ( 1, 0 )
        Left ->            
            ( -1, 0 )

flatten : List (List a) -> List a
flatten list = 
    List.foldr (++) [] list