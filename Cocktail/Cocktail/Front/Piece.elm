module Piece exposing (PieceType(..), Color(..), Vector(..), possibleRegularMoves)

import Set

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

possibleRegularMoves : Color -> PieceType -> (Int, Int) -> Set.Set (Int, Int)
possibleRegularMoves color pieceType coordinates =
    Set.fromList (
        case pieceType of
            King ->            
                fullMoves (regularMoves [Up] coordinates) 1
                ++ fullMoves (regularMoves [Down] coordinates) 1
                ++ fullMoves (regularMoves [Right] coordinates) 1
                ++ fullMoves (regularMoves [Left] coordinates) 1
                ++ fullMoves (regularMoves [Up, Right] coordinates) 1
                ++ fullMoves (regularMoves [Up, Right] coordinates) 1
                ++ fullMoves (regularMoves [Down, Right] coordinates) 1
                ++ fullMoves (regularMoves [Down, Left] coordinates) 1
            Queen ->            
                fullMoves (regularMoves [Up] coordinates) 8
                ++ fullMoves (regularMoves [Down] coordinates) 8
                ++ fullMoves (regularMoves [Right] coordinates) 8
                ++ fullMoves (regularMoves [Left] coordinates) 8
                ++ fullMoves (regularMoves [Up, Right] coordinates) 8
                ++ fullMoves (regularMoves [Up, Right] coordinates) 8
                ++ fullMoves (regularMoves [Down, Right] coordinates) 8
                ++ fullMoves (regularMoves [Down, Left] coordinates) 8
            Bishop ->            
                fullMoves (regularMoves [Up, Right] coordinates) 8
                ++ fullMoves (regularMoves [Up, Left] coordinates) 8
                ++ fullMoves (regularMoves [Down, Right] coordinates) 8
                ++ fullMoves (regularMoves [Down, Left] coordinates) 8
            Knight ->            
                fullMoves (regularMoves [Up, Up, Right] coordinates) 1
                ++ fullMoves (regularMoves [Up, Up, Left] coordinates) 1
                ++ fullMoves (regularMoves [Down, Down, Right] coordinates) 1
                ++ fullMoves (regularMoves [Down, Down, Left] coordinates) 1
                ++ fullMoves (regularMoves [Right, Right, Up] coordinates) 1
                ++ fullMoves (regularMoves [Right, Right, Down] coordinates) 1
                ++ fullMoves (regularMoves [Left, Left, Up] coordinates) 1
                ++ fullMoves (regularMoves [Left, Left, Down] coordinates) 1
            Rook ->        
                fullMoves (regularMoves [Up] coordinates) 8
                ++ fullMoves (regularMoves [Down] coordinates) 8
                ++ fullMoves (regularMoves [Right] coordinates) 8
                ++ fullMoves (regularMoves [Left] coordinates) 8
            Pawn ->
                case color of
                    White ->
                        fullMoves (regularMoves [Up] coordinates) 1
                        ++ fullMoves (regularMoves [Up] coordinates) 2
                        ++ fullMoves (regularMoves [Up, Right] coordinates) 1
                        ++ fullMoves (regularMoves [Up, Left] coordinates) 1
                    Black ->
                        fullMoves (regularMoves [Down] coordinates) 1
                        ++ fullMoves (regularMoves [Down] coordinates) 2
                        ++ fullMoves (regularMoves [Down, Right] coordinates) 1
                        ++ fullMoves (regularMoves [Down, Left] coordinates) 1
    )

fullMoves : (Int, Int) -> Int -> List (Int, Int)
fullMoves moves multiplicator = 
   List.map ( regularMoveToFullMove moves ) ( List.range 1 multiplicator )

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