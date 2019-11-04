module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open System
open Domain
open ClientFunctions
open ClientTypes
open Helper



//############################################################################
//#############################SERVER-COMM####################################
//############################################################################


//############# call highscore list ##############
let callHighscoreList () =
    Thoth.Fetch.Fetch.fetchAs<(int*int*DateTime*Difficulty) list> ("http://localhost:8080/api/callhighscore")
let callHighscoreListCommand ()=
    Cmd.OfPromise.either callHighscoreList () HighscoreCalled ServerError


//############# save highscore ##############
let saveInHighScoreList (highscore:int*int*DateTime*Difficulty) =
    meinPost<int*int*DateTime*Difficulty, string> "http://localhost:8080/api/savehighscore" highscore

let saveInHighScoreListCommand highscore =
    Cmd.OfPromise.either saveInHighScoreList highscore HighscoreSaved ServerError


//############################################################################
//################################MODEL#######################################
//############################################################################



let init () : Model * Cmd<Msg> =
    let initialModel = {
        Field = [[]]
        MaxRndNumber = 0
        StartTime = DateTime.UtcNow
        EndTime = DateTime.UtcNow
        Won = false
        CurrentHighscore = None
        Highscores = []
        FieldDifficulty = NoGame
        //PenaltyCounter = 0
        Error = false
        ErrorMessage = None
        }
    initialModel, Cmd.none



//############################################################################
//###############################UPDATE#######################################
//############################################################################



let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    | ServerError error ->
        Fable.Core.JS.console.log(error)
        let nextModel = {
            currentModel with
                Error = true
                ErrorMessage = Some error.Message
        }
        nextModel, Cmd.none
    | SaveHighscore highscore ->
        currentModel, saveInHighScoreListCommand highscore
    | CallHighscore ->
        currentModel, callHighscoreListCommand ()
    | HighscoreCalled highscoreList ->
        let currentModel = {
            currentModel with
                Highscores =  highscoreList
            }
        currentModel, Cmd.none
    | HighscoreSaved string ->
        currentModel, Cmd.ofMsg CallHighscore // Cmd.none
    //#### and set StartTime
    | SwitchFirstImage newField ->        
        let nextModel = {
            currentModel with
                Field = newField
                StartTime = DateTime.UtcNow
                }
        nextModel, Cmd.none
    | SwitchImage newField ->        
        let nextModel = {
            currentModel with
                Field = newField
                }
        nextModel, Cmd.none
    //####after switching 2nd image, check if a pair is visible
    | SwitchSecondImage newField ->     
        let nextModel = {
            currentModel with
                Field = newField
                }
        let msgCheckPair = Cmd.ofMsg (CheckIfPair nextModel.Field) 
        nextModel, msgCheckPair
    | CheckIfPair fieldAfterSecondSwitch -> isPairAndIsWon fieldAfterSecondSwitch currentModel
    | Cover newList -> coverTiles newList currentModel
    | BuildField (playField, maxRndNumber, difficulty) ->
        buildField (playField, maxRndNumber, difficulty) currentModel
    | Won model -> isWon currentModel model 
      
    

//############################################################################
//################################VIEW########################################
//############################################################################



let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [
            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is8) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is8) ]
                        [
                            p [ Style [CSSProp.FontSize 150; CSSProp.TextAlign TextAlignOptions.Center ] ] [str "Memo - Find the pairs"]
                            p [ Style [CSSProp.TextAlign TextAlignOptions.Center ] ] [str "A note to penalty seconds:"]
                            p
                                [ Style [CSSProp.TextAlign TextAlignOptions.Center ] ]
                                [str "If tiles of one image are uncovered 5 times you get 10 penalty seconds, 5 times get 30 seconds and 7+ times get 60 seconds."]
                        ]
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    chooseFieldSize dispatch
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


            //####playField
            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] 
                    playfield model dispatch
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [] 
                    ] 


            //####highscore annoucement
            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    highscoreannouncement model
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]



            //####highscore tables
            if model.Highscores <> [] then

                Columns.columns
                    [ ][
                        Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                        showHighscores model
                        Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
