open Game

[<EntryPoint>]
let main argv =
    printfn "%s" argv.[0]
    Game.startGame argv.[0]
    0