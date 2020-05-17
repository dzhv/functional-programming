module Communicator

open Decoding
open HttpClient
open System
open System.Text

let baseUrl = "http://tictactoe.homedir.eu/game/"
let urlEnding = "/player/1"
let testUrlEnding = "/player/2"

let postMoves (gameId: string) (movesToPost: string) = 
    let request = 
        createRequest Post <| (baseUrl + gameId + urlEnding)
        |> withHeader (ContentType("application/bencode+map"))
        |> withBody ( movesToPost )
    let response = request |> getResponse
    printfn "POST Status code was: %i" response.StatusCode

let getMoves (gameId: string) : string =
    let request = 
        createRequest Get <| (baseUrl + gameId + urlEnding)
        |> withHeader (Accept("application/bencode+map"))
    getResponseBody request

    