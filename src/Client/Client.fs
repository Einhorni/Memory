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



    
type Model = {
    Field: (int * int) list list // visibility * type of picture
    MaxRndNumber: int
    StartTime: DateTime
    EndTime: DateTime
    Won: bool
    }


type Msg =
    | SwitchFirstImage of (int * int) list list
    | SwitchImage of (int * int) list list
    | SwitchSecondImage of (int * int) list list // oder ein if in switch image, je nachdem of eins offen ist oder keins
    | CheckIfPair of (int * int) list list
    | Cover of (int*int) list list
    | BuildField of ((int*int) list list) * int
    | Won


let init () : Model * Cmd<Msg> =
    let initialModel = {
        Field = [[]] //[[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]]
        MaxRndNumber = 0
        StartTime = DateTime.Now
        EndTime = DateTime.Now
        Won = false
        }
    initialModel, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    //## and set StartTime
    | SwitchFirstImage newField ->        
        let nextModel = {
            currentModel with
                Field = newField
                StartTime = DateTime.Now
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
                    let (visible, image) = y
                    visible = image && visible <> 0
                    ))
            |> List.concat
            |> List.distinct


        //####change model if pair
        let nextModel =
            if checkIfPair.Length = 1 then
                let shownImage =
                    checkIfPair |> List.exactlyOne
                let newField =
                    fieldAfterSecondSwitch
                    //[[0,0; 0,0; 0,0; 0,0]; [0,0; 0,6; 1,0; 6,6]; [7,0; 0,8; 5,0; 1,0]; [2,0; 5,0; 0,3; 7,0]]
                    |> List.map (fun x ->
                        x
                        |> List.map (fun y ->
                            let (visible, image) = y
                            if y = shownImage then (image, 0) else y))
                {
                    currentModel with
                        Field = newField
                }
            else currentModel

        //####wenn nextModel -> is won = 0 dann neue Msg -> endTime

        //#### true = won
        let isWon playField =
            let howManyTilesUntilWon =
                playField
                //[[1,0; 2,0]; [1,0; 2,0]]
                |> List.map (fun x ->
                    x
                    |> List.filter (fun y ->
                        let (visible, image) = y
                        y = (0, image) || visible = image))
                |> List.concat
                |> List.length
            howManyTilesUntilWon = 0
        
        let wonOrInGame = 
            if (isWon nextModel.Field) then (Cmd.ofMsg Won)
            else Cmd.none

        nextModel, wonOrInGame
    | Cover newList ->
        let newNewList =
            newList
            |> List.map (fun x ->
                x
                |> List.map (fun y ->
                    let (a,b) = y
                    if b = 0 && a <> 0 then (a,0) // = y
                    else (0,b)))
        let nextModel = {
            currentModel with
                Field = newNewList
                }
        nextModel, Cmd.none
    | BuildField (playField, maxRndNumber) ->
        let nextModel = {
            currentModel with
                Field = playField
                MaxRndNumber = maxRndNumber
                Won = false
                }
        nextModel, Cmd.none
    | Won ->
        let nextModel = {
            currentModel with
                Won = true
                EndTime = DateTime.Now
            }
        nextModel, Cmd.none

        
let r = Random()


//####visibility and value of image
let showImages modelFeld =
    match modelFeld with
    | (1,_) -> Src "images\Unbenannt2.png"
    | (2,_) -> Src "images\Unbenannt3.png"
    | (3,_) -> Src "images\Unbenannt4.png"
    | (4,_) -> Src "images\Unbenannt5.png"
    | (5,_) -> Src "images\Unbenannt6.png"
    | (6,_) -> Src "images\Unbenannt7.png"
    | (7,_) -> Src "images\Unbenannt8.png"
    | (8,_) -> Src "images\Unbenannt9.png"
    | (9,_) -> Src "images\Unbenannt10.png"
    | (10,_) -> Src "images\Unbenannt11.png"
    | (11,_) -> Src "images\Unbenannt12.png"
    | (12,_) -> Src "images\Unbenannt13.png"
    | (13,_) -> Src "images\Unbenannt14.png"
    | (14,_) -> Src "images\Unbenannt15.png"
    | (_) -> Src "images\Unbenannt.png"
    

//####how many images are already set 
let howManyImagesOfOneType model rndImage =
    model.Field
    |> List.concat
    |> List.filter (fun x -> x = (rndImage, rndImage) || x = (0, rndImage) || x = (rndImage, 0))
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
            //####set new image to tile
            if i = i2 && x = (0,0) then
                setRandomNumberForImages model howManyImagesOfOneType
            //####if number already set, just switch first part of tuple (uncover image without changing set number/image)
            elif i = i2 && x <> (0,0) then
                let (visible, image) = x
                (image, image)
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
            let (visible, image) = y
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
            let (visible, image) = y
            if visible = image && visible <> 0 then (0, image) else y))


//#### true = new Field
let isNewField playField =
    let howManyUnvoveredTiles =
        playField
        //[[(0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0)]]
        |> List.map (fun x ->
            x
            |> List.filter (fun y ->
                y <> (0,0)))
        |> List.concat
        |> List.length
    howManyUnvoveredTiles = 0


//####
let startTimer = DateTime.Now //ich muss irgendwie die anfangszeit speichern (erste karte umgedreht), davon muss ich die endzeit (alle umgedreht) abziehen
let endTimer finished = DateTime.Now


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
                                Button.OnClick (fun _ -> dispatch (BuildField ([[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]], 9)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Size IsLarge
                                Button.Option.Props [ Style [ Margin "20px"] ]

                            ]
                            [ str "Medium: 4x4"]
                        Button.button
                            [
                                Button.OnClick (fun _ -> dispatch (BuildField ([[(0,0);(0,0)]; [(0,0); (0,0)]], 3)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Option.Props [ Style [ Margin "20px"] ]
                                Button.Size IsLarge
                            ]
                            [ str "Easy: 2x2"]
                        Button.button
                            [
                                Button.OnClick (fun _ -> dispatch (BuildField ([[(0,0); (0,0); (0,0); (0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0); (0,0); (0,0); (0,0)]], 15)) )
                                Button.IsOutlined
                                Button.Color IsBlack
                                Button.Option.Props [ Style [ Margin "20px"] ]
                                Button.Size IsLarge
                            ]
                            [ str "Hard: 7x4"]
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
                                                            yield showImages model.Field.[index1].[index2] //visibility and value

                                                            yield OnClick (fun _ ->   
                                                                match (howManyUncovered model) with //(howManyImagesOfOnePairOpen model index1 index2)
                                                                | 1 ->
                                                                    match model.Field.[index1].[index2] with
                                                                    //#### cant click visible image another time, because upper if doesnt work??
                                                                    | (0,_) ->
                                                                        dispatch (SwitchSecondImage (uncoverTiles index1 index2 model))
                                                                    | _ -> ()
                                                                | 0 ->
                                                                    match model.Field.[index1].[index2] with
                                                                    //#### cant click visible image another time, because upper if doesnt work??
                                                                    | (0,_) ->
                                                                        if (isNewField model.Field) then
                                                                            dispatch (SwitchFirstImage (uncoverTiles index1 index2 model))
                                                                        else dispatch (SwitchImage (uncoverTiles index1 index2 model))
                                                                    | _ -> ()
                                                                | 2 ->
                                                                    dispatch (Cover (coverFields model.Field))
                                                                | _ -> ())] //img
                                                        //p [] [ str (sprintf "%A: Anzahl offen" (howManyUncovered model))]
                                        ])])] //tbody
                                ] //table
                        ] //column
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [] 
                    ] //columns


            Columns.columns
                [ ][
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ]
                    Column.column [ Column.Width (Screen.All, Column.Is8) ] [
                                    //time needed for solving
                        p [] [
                            if (model.EndTime > model.StartTime) then
                                let duration = model.EndTime - model.StartTime
                                let minutes = duration.Minutes
                                let seconds = duration.Seconds                    
                                if duration.Minutes > 0 then
                                    yield str (sprintf "%A:%A: gebrauchte Zeit" (duration.Minutes.ToString()) (duration.Seconds.ToString()))
                                else
                                    if seconds < 10 then
                                        let showSeconds = sprintf "0%i" seconds
                                        yield str (sprintf "Won! Your're time is: 00:%A: gebrauchte Zeit" showSeconds)
                            ]   
                    ]
                    Column.column [ Column.Width (Screen.All, Column.Is2) ] [ ] ]


 


            //#####################################   LOGS
            p [] [ str (sprintf "%A : Modell" model.Field)]
            p [] [str (sprintf "%A : Startzeit" model.StartTime )]
            p [] [str (sprintf "%A : Endzeit" model.EndTime )]
            

               
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
