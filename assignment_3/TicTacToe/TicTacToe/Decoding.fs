module Decoding

open FParsec

let msg = "d1:ad1:v1:x1:xi2e1:yi2ee1:bd1:v1:o1:xi2e1:yi0eee"
let dictMsg = "d1:v1:x1:xi2e1:yi2ee"

type Move = 
    struct
        val x: int
        val y: int
        val v: char
        new(xcord: int, ycord: int, value: char)
            = { x = xcord; y = ycord; v = value }
    end

type CharParser = Parser<char, unit>
type StringParser<'t> = Parser<'t, string>

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success"
    | Failure(result, _, _) -> printfn "Failure"


let parseMoveKey : CharParser = (pchar '1') >>. (pchar ':') >>. ((pchar 'v') <|> (pchar 'x') <|> (pchar 'y'))
let parseVValue: CharParser = (pchar '1') >>. (pchar ':') >>. ((pchar 'x') <|> (pchar 'o')) 
let parseCoordinateValue: CharParser = (pchar 'i') >>. (pchar '0' <|> pchar '1' <|> pchar '2') .>> (pchar 'e')

let parseKeyValuePair = parseMoveKey .>>. (parseCoordinateValue <|> parseVValue)

let createMove tuples =
    let (valueKey, value) = List.find (fun (a, b) -> a = 'v') tuples
    let (xKey, xValue) = List.find (fun (a, b) -> a = 'x') tuples
    let (yKey, yValue) = List.find (fun (a, b) -> a = 'y') tuples
    let x = System.Int32.Parse(xValue.ToString())
    let y = System.Int32.Parse(yValue.ToString())
    new Move(x, y, value)

let parseSingleMoveDictionary = (pchar 'd') >>. (many parseKeyValuePair) .>> (pchar 'e') |>> createMove

let parseAnyKey: CharParser = (pchar '1') >>. (pchar ':') >>. anyChar
let parseKeyAndMove = parseAnyKey >>. parseSingleMoveDictionary

let parseAllMoves = (many parseKeyAndMove)
let parseExternalDictionary = (pchar 'd') >>. parseAllMoves .>> (pchar 'e')

let decodeMessage = run parseExternalDictionary

let moveToString (move : Move) : string = 
    ( move.v.ToString() + move.x.ToString() + move.y.ToString() )

