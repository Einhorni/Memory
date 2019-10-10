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


open System


type Visibility =
    | Visible
    | Invisible

type Matched =
    | Matched
    | Unmatched
    

type Model = {
    A1: Visibility * Matched
    Feld: (int * int) list list // visibility * type of picture
    }

type Msg =
    | SwitchImage of (int * int) list list
    | CoverAll of (int*int) list list


let init () : Model * Cmd<Msg> =
    let initialModel = {
        A1 = Invisible, Unmatched
        Feld = [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
        }
    initialModel, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    | SwitchImage newList ->        
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = newList
                }
        nextModel, Cmd.none
    | CoverAll newList ->
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = newList
                }
        nextModel, Cmd.none

        
let r = Random()


//####visibility and value of image
let chooseImages modelFeld =
    match modelFeld with
    | (1,_) -> Src "images\Unbenannt2.png"
    | (2,_) -> Src "images\Unbenannt3.png"
    | (3,_) -> Src "images\Unbenannt4.png"
    | (4,_) -> Src "images\Unbenannt5.png"
    | (5,_) -> Src "images\Unbenannt6.png"
    | (6,_) -> Src "images\Unbenannt7.png"
    | (7,_) -> Src "images\Unbenannt8.png"
    | (8,_) -> Src "images\Unbenannt9.png"
    | (_) -> Src "images\Unbenannt.png"
    

//####how many images are already set 
//let tuple = 0,3
let howManyImagesOfOneType model rndNumber =
    //[[0,5; 0,5; 0,8; 0,5]; [0,3; 0,1; 0,4; 0,1]; [0,5; 0,3; 0,5; 0,3]; [0,7; 0,4; 0,3; 0,4]]
    model.Feld
    |> List.concat
    |> List.filter (fun x -> x = (rndNumber, rndNumber) || x = (0, rndNumber))
    |> List.length


//####how many images are already set 
let howManyImagesOfOnePairOpen model index1 index2 =
    let (visible, image) = model.Feld.[index1].[index2]
    model.Feld
    |> List.concat
    |> List.filter (fun x -> x = (image,image) && x <> (0,0))
    |> List.length


//####set random number while not 2 same numbers --> if 2, then new random number    
let rec setRandomNumberForImages model howManyImagesOfOneType = 
    let rndNumber = r.Next(1,9)
    match (howManyImagesOfOneType model rndNumber) with
    | 2 -> setRandomNumberForImages model (howManyImagesOfOneType) 
    | _ -> (rndNumber, rndNumber)


////####uncover already set random number (image)
//let unvocerImage (model:Model) i1 i2 =
//    model.Feld.[i1].[i2] //exaktes feld
    

//####build new list for model
let newListListrandomImages i1 i2 model =                                                        
    let innereListe = model.Feld.[i1]
    let neueInnereListe =
        innereListe
        |> List.mapi (fun i x ->
            if i = i2 && x = (0,0) then
                setRandomNumberForImages model howManyImagesOfOneType
            //####if number already set, just switch first part of tuple (uncover image without changing set number/image)
            elif i = i2 && x <> (0,0) then
                let (visible, image) = x
                (image, image)
            else x)
    let neueÄußereListe =
        model.Feld
        |> List.mapi (fun i x -> if i = i1 then neueInnereListe else x)
    neueÄußereListe

//####how many uncovered?
let uncovered model =
    model.Feld
    |> List.map (fun x ->
        x
        |> List.filter (fun y ->
            y <> (0,0) &&
            y <> (0,1) &&
            y <> (0,2) &&
            y <> (0,3) &&
            y <> (0,4) &&
            y <> (0,5) &&
            y <> (0,6) &&
            y <> (0,7) &&
            y <> (0,8)
            ))
        |> List.concat
    |> List.length


//#### set uncovered to covered
let coverFields feld =

    feld
    |> List.map (fun x ->
        x
        |> List.map (fun y ->
            match y with
                
            | (1,1) -> (0,1)
            | (2,2) -> (0,2)
            | (3,3) -> (0,3)
            | (4,4) -> (0,4)
            | (5,5) -> (0,5)
            | (6,6) -> (0,6)
            | (7,7) -> (0,7)
            | (8,8) -> (0,8)
            | _ -> y))

let createNewListe feld =
                                                                
    feld
    |> List.map (fun x ->
        x
        |> List.map (fun y ->
            let (visible, value) = y
            (visible, 0)))
    


let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ 
            table [ Style
                [
                    CSSProp.Border "1 solid"
                    
                ] ]
                [
                    tbody [ ]
                        [
                            yield! model.Feld
                            |> List.mapi (fun index1 list ->
                                tr [][
                                    yield! list
                                    |> List.mapi (fun index2 tuple ->
                                        td
                                            []
                                            [
                                                
                                                img
                                                    [
                                                        
                                                        chooseImages model.Feld.[index1].[index2] //visibility and value
                                                        

                                                        OnClick (fun _ ->
                                                            dispatch (SwitchImage (newListListrandomImages index1 index2 model))

                                                            if (uncovered model) = 2 //&& (howManyImagesOfOnePairOpen model index1 index2) <> 2 //funktioniert nicht
                                                            then dispatch (CoverAll (coverFields model.Feld))


                                                            elif (howManyImagesOfOnePairOpen model index1 index2) = 2 //if funktioniert nicht --> nochmal prüfen
                                                            then
                                                                
                                                                dispatch (CoverAll (coverFields (createNewListe model.Feld) ))
                                                                //neue liste mit betroffenen feldern auf (zahl,0) machen
                                                                //dispatch cover neueListe
                                                                    
                                                            )
                                                        
                                                    ]
                                                str (sprintf "%A" (howManyImagesOfOnePairOpen model index1 index2))
                                                str (sprintf "%A" (uncovered model))
                                                    ])
                                               
                                                
                                            
                                        ])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     





                        ]
                ]
            p [] [ str (sprintf "%A" model.Feld)]
            p [] [ str (sprintf "%A" (createNewListe model.Feld))]
                 
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
