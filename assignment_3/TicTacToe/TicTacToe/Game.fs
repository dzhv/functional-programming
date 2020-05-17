module Game

open Decoding
open Encoding
open FParsec
open Communicator

exception BadMessage of string

let predefinedMoves = [
    new Move(0, 0, 'x');
    new Move(0, 2, 'x');
    new Move(1, 0, 'x');
    new Move(1, 2, 'x');
    new Move(2, 1, 'x')]
    
let moves = 
    match (decodeMessage msg) with
    | Success(moveArray, _, _) -> moveArray
    | Failure(errorMsg, _, _) -> raise (BadMessage(errorMsg))

let message = encodeMoveArrayToDictionary moves

let rec playGame (gameId: string) (remainingMoves: List<Move>) (playedMoves: List<Move>) =
    match remainingMoves with
    | [] -> printfn "The game has been played succesfully"
    | [lastMove] -> 
        let movesToSend = playedMoves @ [lastMove]
        let stringToSend = encodeMoveArrayToDictionary movesToSend
        postMoves gameId stringToSend
        playGame gameId [] []
    | currentMove::remainingMoves ->
        let movesToSend = playedMoves @ [currentMove]
        let stringToSend = encodeMoveArrayToDictionary movesToSend
        postMoves gameId stringToSend

        let movesString = getMoves gameId
        match decodeMessage movesString with
            | Success (moves, _, _) -> playGame gameId remainingMoves moves
            | Failure (errorMsg, _, _) -> raise (BadMessage(errorMsg))

let startGame (gameId: string) = playGame gameId predefinedMoves []