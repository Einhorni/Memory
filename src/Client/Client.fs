module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Shared
open Fable.Import
open System
open Fulma
open System.Net.Security
open Fable.FontAwesome.Free
open System.Globalization
open Fetch.Types
open Fetch
open Domain
open Fable.DateFunctions
open Fable.Import


    
type Model = {
    Field: (int * int * int) list list // visibility * type of picture
    MaxRndNumber: int
    StartTime: DateTime
    EndTime: DateTime
    Won: bool
    FieldDifficulty: Difficulty 
    CurrentHighscore: (int*int*DateTime*Difficulty) option //minutes, seconds, Date, field difficulty
    Highscores: (int*int*DateTime*Difficulty) list
    //PenaltyCounter: int
    Error: bool
    ErrorMessage: string option
    }


type Msg =
    | ServerError of exn
    | SaveHighscore of (int*int*DateTime*Difficulty)
    | CallHighscore
    | HighscoreCalled of (int*int*DateTime*Difficulty) list
    | HighscoreSaved of string //oder eher kein string, bzw. nichts mitgeben
    | SwitchFirstImage of (int * int * int) list list
    | SwitchImage of (int * int * int) list list
    | SwitchSecondImage of (int * int * int) list list
    | CheckIfPair of (int * int * int) list list
    | Cover of (int * int * int) list list
    | BuildField of ((int * int * int) list list) * int * Difficulty
    | Won of Model



let inline meinPost<'typDerZuVersendendenDaten, 'typDerZuErhaltendenDaten> adresse (datenVonClientDieIchVersendenWill:'typDerZuVersendendenDaten) =
    promise {
        let zuVersendendeDatenImJSONFormat = Thoth.Json.Encode.toString 0 datenVonClientDieIchVersendenWill
        let! responseDesRequests =
            GlobalFetch.fetch (RequestInfo.Url adresse, requestProps
                [
                    //fetch sendet Daten als Post im Json Format an Server
                    //fetch ist eine Funktion liefert die antwort des servers (response), falls es einen server gibt
                    Fetch.Types.Method Fetch.Types.HttpMethod.POST 
                    Fetch.requestHeaders [Fetch.Types.ContentType "application/json"] 
                    Fetch.Types.Body (unbox(zuVersendendeDatenImJSONFormat))
                ])
        if responseDesRequests.Ok then
            if typeof<'typDerZuErhaltendenDaten> = typeof<unit> then
                //standardwert des typsDerZuErhaltendenDaten zurückgeben... weil auf unit geprüft wird, kommt hier auch immer unit zurück
                return Unchecked.defaultof<'typDerZuErhaltendenDaten>
            else
                //antwort des servers in textform = json, weil wir das wissen, weil server in json antwortet
                let! antwortDesServers = responseDesRequests.text()
                //Decoder: damit json (antwortdesServers) wieder in das richtige Format (F-Sharp Typ) umgewandelt wird
                let zuErhaltendeDaten = Decode.Auto.fromString<'typDerZuErhaltendenDaten> antwortDesServers
                match zuErhaltendeDaten with
                // wenn Umwandlung von json zum Rückgabetyp erfolgreich war - - antwortvomserver = echtes Rückgabeobjekt, brauchbare Daten
                | Ok antwortVomServer -> return antwortVomServer
                //bei Problemen mit Umwandlung Json zum Rückgabewerttyp im Client
                //später kann man ersetzen mit Hilfe von: String.StartsWith("technisch")
                | Error fehlerMeldungJsonUmwandlung -> return failwith (sprintf "technisch: %s" fehlerMeldungJsonUmwandlung)
        else
            let! antwortDesServers = responseDesRequests.text()
            //response hat nicht geklappt: fehlermeldung ist vom Typ string (das eigentlich Json-String ist = von F-sharp nicht verstanden wird -> F#-String)
            //im server formulierte fehlermeldungen
            let zuErhaltendeFehlermeldung = Decode.Auto.fromString<string> antwortDesServers
            match zuErhaltendeFehlermeldung with
            | Ok zuErhaltendeFehlermeldung -> return failwith zuErhaltendeFehlermeldung
            //bei Problemen mit Umwandlung Json zum Rückgabewerttyp im Client
            //später kann man ersetzen mit Hilfe von: String.StartsWith("technisch")
            | Error fehlerMeldungJsonUmwandlung -> return failwith (sprintf "technisch: %s" fehlerMeldungJsonUmwandlung)          
    }

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


let penaltyCalculator (field: (int*int*int) list list) =
    field
    |> List.map (fun y ->
        y
        |> List.map (fun x ->
        let (a,b, counter) = x
        counter)
        |> List.map (fun x ->
            match x with
            | 3 -> 5
            | 4 -> 10
            | 5 -> 20
            | x when x > 6 -> 60
            | _ -> 0)
        |> List.sum)
    |> List.sum


let newHighscore model interModel = 
    let duration = interModel.EndTime.AddSeconds(penaltyCalculator model.Field) - model.StartTime
    let minutes = duration.Minutes
    let seconds = duration.Seconds                    
            
    (minutes, seconds, DateTime.UtcNow, model.FieldDifficulty)


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
    | CheckIfPair fieldAfterSecondSwitch ->
        //####pair if 2 tuples with same values, e.g. (1,1); (1,1) = pair -- (1,1); (2,2) <> pair
        //####pair if = 1
        let checkIfPair =
            fieldAfterSecondSwitch
            |> List.map (fun x ->
                x
                |> List.filter (fun y ->
                    let (visible, image, counter) = y
                    visible = image && visible <> 0
                    ))
            |> List.concat
            |> List.map (fun x ->
                let (visible, image, counter) = x
                (visible, image)
                )
            |> List.distinct


        //####change model if pair
        let nextModel =
            if checkIfPair.Length = 1 then
                let (visiblePair, imagePair) =
                    checkIfPair |> List.exactlyOne
                let newField =
                    fieldAfterSecondSwitch
                    |> List.map (fun x ->
                        x
                        |> List.map (fun y ->
                            let (visible, image, counter) = y
                            if visible = visiblePair && image = imagePair then (image, 0, counter) else y))
                {
                    currentModel with
                        Field = newField
                }
            else currentModel

        //#### true = won
        let isWon playField =
            let howManyTilesUntilWon =
                playField
                |> List.map (fun x ->
                    x
                    |> List.filter (fun y ->
                        let (visible, image, counter) = y
                        y = (0, image, counter) || visible = image))
                |> List.concat
                |> List.length
            howManyTilesUntilWon = 0
        
        let wonOrInGame = 
            if (isWon nextModel.Field) then (Cmd.ofMsg (Won nextModel))
            else Cmd.none

        nextModel, wonOrInGame
    | Cover newList ->
        let newNewList =
            newList
            |> List.map (fun x ->
                x
                |> List.map (fun y ->
                    let (a,b,c) = y
                    if b = 0 && a <> 0 then (a,0,c) // = y
                    else (0,b,c)))
        let nextModel = {
            currentModel with
                Field = newNewList
                }
        nextModel, Cmd.none
    | BuildField (playField, maxRndNumber, difficulty) ->
        let nextModel = {
            currentModel with
                Field = playField
                MaxRndNumber = maxRndNumber
                Won = false
                Error = false
                FieldDifficulty = difficulty
                CurrentHighscore = None
                Highscores = []
                }
        nextModel, Cmd.none
    | Won model ->
        let interModel = {
            currentModel with
                Won = true
                EndTime = DateTime.UtcNow
            }
        let nextModel = {
            interModel with
                CurrentHighscore = Some (newHighscore model interModel)
            }
        let cmdSaveHighscore = Cmd.ofMsg (SaveHighscore (newHighscore model interModel))
        nextModel, cmdSaveHighscore

        
let r = Random()


//####visibility and value of image
let showImages modelFeld =
    match modelFeld with
    | (1,_,_) -> Src "images\Unbenannt2.png"
    | (2,_,_) -> Src "images\Unbenannt3.png"
    | (3,_,_) -> Src "images\Unbenannt4.png"
    | (4,_,_) -> Src "images\Unbenannt5.png"
    | (5,_,_) -> Src "images\Unbenannt6.png"
    | (6,_,_) -> Src "images\Unbenannt7.png"
    | (7,_,_) -> Src "images\Unbenannt8.png"
    | (8,_,_) -> Src "images\Unbenannt9.png"
    | (9,_,_) -> Src "images\Unbenannt10.png"
    | (10,_,_) -> Src "images\Unbenannt11.png"
    | (11,_,_) -> Src "images\Unbenannt12.png"
    | (12,_,_) -> Src "images\Unbenannt13.png"
    | (13,_,_) -> Src "images\Unbenannt14.png"
    | (14,_,_) -> Src "images\Unbenannt15.png"
    | (_) -> Src "images\Unbenannt.png"
    

//####how many images are already set 
let howManyImagesOfOneType model rndImage =
    model.Field
    |> List.concat
    |> List.filter (fun x ->
        let (a,b,counter) = x
        x = (rndImage, rndImage, counter) || x = (0, rndImage, counter) || x = (rndImage, 0, counter))
    |> List.length


//####set random number(image) while not 2 same numbers(image) --> if 2, then new random number(image)    
let rec setRandomNumberForImages model howManyImagesOfOneType = 
    let rndNumber = r.Next(1,model.MaxRndNumber)
    match (howManyImagesOfOneType model rndNumber) with
    | 2 -> setRandomNumberForImages model (howManyImagesOfOneType) 
    | _ -> (rndNumber, rndNumber)
    

//####uncover tiles for SwitchCover
let uncoverTiles i1 i2 model =                                                        
    let innerList = model.Field.[i1]
    let newInnerList =
        innerList
        |> List.mapi (fun i x ->
            let (visible, image, counter) = x
            //####set new image to tile and increase counter by 1
            if i = i2 && x = (0,0,0) then
                let (newVisible, newImage) = setRandomNumberForImages model howManyImagesOfOneType
                (newVisible, newImage, counter+1)
            //####if number already set, just switch first part of tuple (uncover image without changing set number/image) and increase counter by 1
            elif i = i2 && x <> (0,0, counter) then
                (image, image, counter+1)
            else x)
    let newField =
        model.Field
        |> List.mapi (fun i x -> if i = i1 then newInnerList else x)
    newField


//####how many uncovered?
let howManyUncovered model =
    model.Field
    |> List.map (fun x ->
        x
        |> List.filter (fun y ->
            let (visible, image, counter) = y
            visible = image && visible <> 0 // y = (1,1) || (2,2) ....
            ))
        |> List.concat
    |> List.length


//#### set uncovered to covered, remeber images
let coverFields feld =

    feld
    |> List.map (fun x ->
        x
        |> List.map (fun y ->
            let (visible, image, counter) = y
            if visible = image && visible <> 0 then (0, image, counter) else y))


//#### true = cover all and set new field
let isNewField playField =
    let howManyUnvoveredTiles =
        playField
        |> List.map (fun x ->
            x
            |> List.filter (fun y ->
                //if at least one image is set its not a new field
                y <> (0,0,0))) 
        |> List.concat
        |> List.length
    howManyUnvoveredTiles = 0


//#### higscorelists, 3 difficulties
let savedHighscores model =
    let (highscoresEasy, highscoresRest) =
        model.Highscores
        |> List.partition (fun x ->
            let (a,b,c,difficulty) = x
            difficulty = Easy)

    let highscoresMedium, highscoresHard =
        highscoresRest
        |> List.partition (fun x ->
            let (a,b,c,difficulty) = x
            difficulty = Medium)

    (highscoresEasy, highscoresMedium, highscoresHard)


//#### sort highscoreLists
let sortHighscore highscore =
    highscore
    |> List.sort


//#### mark current score in Highscore List - comparison between two DateTimes doesnt work, therefore the partioning
let markCurrentScore model (savedScore:int*int*DateTime*Difficulty) =
    let (minutes, seconds, date, difficulty) = savedScore
    match model.CurrentHighscore with
    | Some hs ->
        let (currentHsMin, currentHsSec, currentHsDate,c) = hs
        Fable.Core.JS.console.log(sprintf "Hour: %i und %i" currentHsDate.Hour date.Hour)
        if
            currentHsDate.Day = date.Day &&
            currentHsDate.Month = date.Month &&
            currentHsDate.Year = date.Year &&
            currentHsDate.Hour = date.Hour &&
            currentHsDate.Minute = date.Minute &&
            currentHsDate.Second = date.Second &&
            currentHsMin = minutes &&
            currentHsSec = seconds
        then
            ClassName "is-selected"
        else ClassName ""
    | None -> ClassName ""


//####build highscore table
let highscoreTable (highscoreList:(int*int*DateTime*Difficulty) list) (model:Model) =
    Table.table
        [  ] 
        [
        thead [ ]
            [ tr [ ]
                    [
                        th [ ][ str (sprintf "Time") ]
                        th [ ][ str (sprintf "Date") ]
                    ] ]
        tbody []
            [         
                yield! sortHighscore highscoreList
                |> List.map (fun highscore ->
                    let (minutes, seconds, date, difficulty) = highscore
                    tr [ if model.CurrentHighscore = Some highscore then ClassName "is-selected" ]
                        [ 
                            td [ markCurrentScore model highscore ]
                                [
                                    if seconds < 10 then
                                        let secondsString = sprintf "0%i" seconds
                                        str (sprintf "%i:%s" minutes secondsString )
                                    else
                                        str (sprintf "%i:%i" minutes seconds )
                                ]
                            td []
                                [ str (sprintf "%A / %A / %A" date.Day date.Month date.Year ) ]
                        ])
            ]
        ]



    


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
                    Column.column [ Column.Width (Screen.All, Column.Is8) ] [
                        //####Choose size of playfield
                        Button.button
                            [
                                Button.OnClick (fun _ ->
                                    dispatch
                                        (BuildField ([[(0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                            [(0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                            [(0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                            [(0,0,0); (0,0,0); (0,0,0); (0,0,0)]], 9, Medium)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Size IsLarge
                                Button.Option.Props [ Style [ Margin "20px"] ]

                            ]
                            [ str "Medium: 4x4"]
                        Button.button
                            [
                                Button.OnClick (fun _ -> dispatch (BuildField ([[(0,0,0);(0,0,0)]; [(0,0,0); (0,0,0)]], 3, Easy)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Option.Props [ Style [ Margin "20px"] ]
                                Button.Size IsLarge
                            ]
                            [ str "Easy: 2x2"]
                        Button.button
                            [
                                Button.OnClick (fun _ ->
                                    dispatch
                                        (BuildField ([[(0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                                [(0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                                [(0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0)];
                                                [(0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0); (0,0,0)]], 15, Hard)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Option.Props [ Style [ Margin "20px"] ]
                                Button.Size IsLarge
                            ]
                            [ str "Hard: 7x4"]
                        Button.button
                            [
                                Button.OnClick (fun _ -> dispatch CallHighscore )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Option.Props [ Style [ Margin "20px"] ]
                                Button.Size IsLarge
                            ]
                            [ str "Highscores"]
                        ]
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] 
                    Column.column [ Column.Width (Screen.All, Column.Is8) ] [
                            //####playField
                            table
                                [ Style [ CSSProp.Margin "auto" ] ] [
                                    tbody [ ] [
                                        yield! model.Field
                                        |> List.mapi (fun index1 list ->
                                            tr [][
                                                yield! list
                                                |> List.mapi (fun index2 tuple ->
                                                    td [ Style [ CSSProp.Padding "10px" ] ] [
                                                        img [
                                                            showImages model.Field.[index1].[index2]
                                                            OnClick (fun _ ->   
                                                                match (howManyUncovered model) with
                                                                | 1 ->
                                                                    match model.Field.[index1].[index2] with
                                                                    //#### cant click visible image another time
                                                                    | (0,_,_) ->
                                                                        dispatch (SwitchSecondImage (uncoverTiles index1 index2 model))
                                                                    | _ -> ()
                                                                | 0 ->
                                                                    match model.Field.[index1].[index2] with
                                                                    //#### cant click visible image another time
                                                                    | (0,_,_) ->
                                                                        if (isNewField model.Field) then
                                                                            dispatch (SwitchFirstImage (uncoverTiles index1 index2 model))
                                                                        else dispatch (SwitchImage (uncoverTiles index1 index2 model))
                                                                    | _ -> ()
                                                                | 2 ->
                                                                    dispatch (Cover (coverFields model.Field))
                                                                | _ -> ())] //img
                                        ])])] //tbody
                                ] //table
                        ] //column
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [] 
                    ] //columns


            //####highscore annoucement
            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is8) ] [
                                    //time needed for solving
                        p [ Style [CSSProp.TextAlign TextAlignOptions.Center; CSSProp.FontSize 40; CSSProp.Color "black" ] ] [
                            if model.Won = true then
                                let duration = model.EndTime - model.StartTime
                                let minutes = duration.Minutes
                                let seconds = duration.Seconds
                                
                                if seconds < 10 then
                                    let secondsString = sprintf "0%i" seconds
                                    str (sprintf "Won! Your time is: 00:%A" secondsString)
                                    p [ ] [str (sprintf "Your penalty time is: %A seconds" (penaltyCalculator model.Field))]
                                    p [ ] [
                                        match model.CurrentHighscore with
                                        | Some (min, sec, a, b) ->
                                            if seconds < 10 then
                                                let secondsString = sprintf "0%i" seconds
                                                str (sprintf "Your score is: 00:%A" secondsString)
                                            else str (sprintf "Your score is: %A:%A" min sec)
                                        | None -> ()        
                                        ]
                                else
                                    str (sprintf "Won! Your time is: %A:%A" minutes seconds)
                                    p [ ] [str (sprintf "Your penalty time is: %A seconds" (penaltyCalculator model.Field))]
                                    p [ ] [
                                        match model.CurrentHighscore with
                                        | Some (min, sec, a, b) -> str (sprintf "Your score is: %A:%A" min sec)
                                        | None -> ()        
                                        ]
                            ]
                    ]
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]



            //####highscore tables
            if model.Highscores <> [] then

                Columns.columns
                    [ ][
                        Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                        Column.column [ Column.Width (Screen.All, Column.Is8) ] [
                           
                                table
                                    [ Style [CSSProp.Margin "auto"] ]
                                    [ tbody  []
                                     [tr [ ]
                                        [
                                            let (highscoreEasy, highscoresMedium, highscoresHard) = savedHighscores model
                                            td [ Style [ Padding "50px"] ] 
                                                [
                                                    p [ Style [ CSSProp.FontWeight "bold"; CSSProp.TextDecoration "underline" ] ] [str "Easy"]
                                                    highscoreTable highscoreEasy model
                                                ]
                                            td [ Style [ Padding "50px"] ]
                                                [
                                                    p [ Style [ CSSProp.FontWeight "bold"; CSSProp.TextDecoration "underline" ] ] [str "Medium"]
                                                    highscoreTable highscoresMedium model
                                                ]
                                            td [ Style [ Padding "50px"] ]
                                                [
                                                    p [ Style [ CSSProp.FontWeight "bold"; CSSProp.TextDecoration "underline" ] ] [str "Hard"]
                                                    highscoreTable highscoresHard model
                                                ]
                                        ]]]
                              
                        ]
                        Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is8) ]
                        [

                            
                            //#####################################   LOGS
                            p [] [ str (sprintf "%A : Modell" model.Field)]
                            p [] [ str (sprintf "%A : Highscore" model.CurrentHighscore)]
                            p [] [ str (sprintf "%A : Highscores" model.Highscores)]

                            let checkIfPair model =
                                model.Field
                                |> List.map (fun x ->
                                    x
                                    |> List.filter (fun y ->
                                        let (visible, image, counter) = y
                                        visible = image && visible <> 0
                                        ))
                                |> List.concat
                                |> List.distinct

                            p [] [ str (sprintf "%A : checkifpair" (checkIfPair model))]                            

                            

                        ]
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
