module Piece exposing (Coordinates, PieceType(..), Color(..), Vector(..), possibleRegularMoves)

import List.Unique

type alias Coordinates = 
    { x : Int
    , y : Int
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

possibleRegularMoves : Color -> PieceType -> Coordinates -> List Coordinates
possibleRegularMoves color pieceType coordinates =
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

fullMoves : Coordinates -> Int -> List Coordinates
fullMoves moves multiplicator = 
    List.Unique.filterDuplicates (List.map (regularMoveToFullMove moves) (List.range 1 multiplicator))

regularMoveToFullMove : Coordinates -> Int -> Coordinates
regularMoveToFullMove coordinates multiplicator =
    Coordinates ( coordinates.x * (multiplicator - 1) + coordinates.x ) ( coordinates.y * (multiplicator - 1) + coordinates.y)

regularMoves : List Vector -> Coordinates -> Coordinates
regularMoves vectors coordinates =
    List.foldl reduceRegularMoves (Coordinates 0 0) (List.map (vectorToRegularMove coordinates) vectors)

reduceRegularMoves : Coordinates -> Coordinates -> Coordinates
reduceRegularMoves coordinatesA coordinatesB =
    Coordinates (coordinatesA.x + coordinatesB.x) (coordinatesA.y + coordinatesB.y)

vectorToRegularMove : Coordinates -> Vector -> Coordinates
vectorToRegularMove coordinates vector = 
    case vector of
        Up ->            
            ( Coordinates 0 1 )
        Down ->            
            ( Coordinates 0 -1 )
        Right ->            
            ( Coordinates 1 0 )
        Left ->            
            ( Coordinates -1 0 )

flatten : List (List a) -> List a
flatten list = 
    List.foldr (++) [] list