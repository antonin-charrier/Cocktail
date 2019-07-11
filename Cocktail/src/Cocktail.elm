type alias Coordinates = 
    { x : Int
    , y : Int
    }

type Piece
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

possibleRegularMoves : Color -> Piece -> Coordinates -> List (List Coordinates)
possibleRegularMoves color piece coordinates =
    case piece of
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

fullMoves : List Coordinates -> Int -> List (List Coordinates)
fullMoves moves multiplicator = 
    List.map (regularMoveToFullMove moves) (List.range 1 multiplicator)

regularMoveToFullMove : List Coordinates -> Int -> List Coordinates
regularMoveToFullMove moves multiplicator =
    List.map ( regularMoveToMultipliedMove multiplicator ) moves

regularMoveToMultipliedMove : Int -> Coordinates -> Coordinates
regularMoveToMultipliedMove multiplicator coordinates =
    Coordinates ( coordinates.x * multiplicator ) ( coordinates.y * multiplicator )

regularMoves : List Vector -> Coordinates -> List Coordinates
regularMoves vectors coordinates =
    List.map (vectorToRegularMove coordinates) vectors

vectorToRegularMove : Coordinates -> Vector -> Coordinates
vectorToRegularMove coordinates vector = 
    case vector of
        Up ->            
            { coordinates | y = coordinates.y + 1 }
        Down ->            
            { coordinates | y = coordinates.y - 1 }
        Right ->            
            { coordinates | x = coordinates.x + 1 }
        Left ->            
            { coordinates | x = coordinates.x - 1 }