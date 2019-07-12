module Persist

open Suave
open Suave.Json
open System.Runtime.Serialization

[<DataContract>]
type Color
    = White
    | Black

[<DataContract>]
type Piece
    = King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn

let testlist = ["King"; "Queen"; "Pawn";]

let save piecelist = 
    use file = System.IO.File.CreateText("C:\Dev\Prog_Fonct\save")
    piecelist |> List.iter(fun elem -> fprintf file "%s " elem+"/n")
    fprintfn file ""

save testlist