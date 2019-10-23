open DataAccess

open System.IO
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open System
open Domain

//open Shared


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {
    get "/api/callhighscore" (fun next ctx ->
        task {
            let highScoreList = callHighscores "highscores.json"
            return! json highScoreList next ctx
        })

//saveHighscore newList highscore

    post "/api/savehighscore" (fun next ctx ->
        task {
            let! requestWithHighscore = Saturn.ControllerHelpers.Controller.getJson<int*int*DateTime*Difficulty> ctx

            if System.IO.File.Exists("highscores.json") then

                let SavedHighscores = callHighscores "highscores.json"
                let newHighscoreList = saveHighscoreInList SavedHighscores requestWithHighscore
                saveHighscore "highscores.json" newHighscoreList
                ctx.Response.StatusCode <- 200
                return! json "formeller Rückgabestring" next ctx 

            else
                let newHighscoreList = saveHighscoreInList [] requestWithHighscore
                saveHighscore "highscores.json" newHighscoreList
                ctx.Response.StatusCode <- 200
                return! json "formeller Rückgabestring" next ctx 
        })



    
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app


