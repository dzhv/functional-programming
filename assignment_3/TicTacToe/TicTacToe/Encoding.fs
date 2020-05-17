module Encoding

open Decoding

let summsg = "d1:ad1:v1:x1:xi2e1:yi2ee1:bd1:v1:o1:xi2e1:yi0eee"

let encodeMoveToDictionary (move: Move) : string =
    "d1:v1:" + move.v.ToString() + "1:xi" + move.x.ToString() + "e1:yi" + move.y.ToString() + "e"

let rec encodeMoveArrayRecursively (moves: List<Move>) (counter: int) (acc: string) : string =
    match moves with
    | [] -> acc
    | move::remainingMoves -> 
        let str = acc + "1:" + counter.ToString() + encodeMoveToDictionary move + "e"
        encodeMoveArrayRecursively remainingMoves (counter + 1) str

let encodeMoveArrayToDictionary (moves: List<Move>) : string = 
    "d" + encodeMoveArrayRecursively moves 0 "" + "e"