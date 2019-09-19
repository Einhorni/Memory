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

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server

type Visibility =
    | Visible
    | Invisible

type Matched =
    | Matched
    | Unmatched
    

type Model = {
    A1: Visibility * Matched
    Feld: (int * int) list list
    }

type Msg =
    | Bildwechsel of (int * int) list list
    | AlleZudecken of (int*int) list list


let initialCounter () = Fetch.fetchAs<Counter> "/api/init"

let init () : Model * Cmd<Msg> =
    let initialModel = {
        A1 = Invisible, Unmatched
        Feld = [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
        }
    initialModel, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    | Bildwechsel newList ->
        
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = newList
                }
        nextModel, Cmd.none
    | AlleZudecken newList ->
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = newList
                }
        nextModel, Cmd.none
        
let r = Random()

let chooseImages modelFeld =
    match modelFeld with
    | (1,1) -> Src "images\Unbenannt2.png"
    | (2,2) -> Src "images\Unbenannt3.png"
    | (3,3) -> Src "images\Unbenannt4.png"
    | (4,4) -> Src "images\Unbenannt5.png"
    | (5,5) -> Src "images\Unbenannt6.png"
    | (6,6) -> Src "images\Unbenannt7.png"
    | (7,7) -> Src "images\Unbenannt8.png"
    | (8,8) -> Src "images\Unbenannt9.png"
    | (_) -> Src "images\Unbenannt.png"
    
    
let newListListrandomImages i1 i2 model =                                                        
    let innereListe = model.Feld.[i1]
    let neueInnereListe =
        innereListe
        |> List.mapi (fun i x ->
            if i = i2 then
                let rndNumber = r.Next(1,9)
                (rndNumber, rndNumber)
            else x)
    let neueÄußereListe =
        model.Feld
        |> List.mapi (fun i x -> if i = i1 then neueInnereListe else x)
    neueÄußereListe


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


let coverFields model =
    
    model.Feld
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
    
//[[0,5; 0,5; 0,8; 0,5]; [0,3; 0,1; 0,4; 0,1]; [0,5; 0,3; 0,5; 0,3]; [0,7; 0,4; 0,3; 0,4]]
//|> List.map (fun x ->
//    x
//    |> List.)
    


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
                                                        yield chooseImages model.Feld.[index1].[index2]
                                                        
                                                        if //damit nicht doppelt gedrückt werden kann
                                                            model.Feld.[index1].[index2] = (0,0) ||
                                                            model.Feld.[index1].[index2] = (0,1) ||
                                                            model.Feld.[index1].[index2] = (0,2) ||
                                                            model.Feld.[index1].[index2] = (0,3) ||
                                                            model.Feld.[index1].[index2] = (0,4) ||
                                                            model.Feld.[index1].[index2] = (0,5) ||
                                                            model.Feld.[index1].[index2] = (0,6) ||
                                                            model.Feld.[index1].[index2] = (0,7) ||
                                                            model.Feld.[index1].[index2] = (0,8)
                                                        then 
                                                            yield OnClick (fun _ ->
                                                                dispatch (Bildwechsel (newListListrandomImages index1 index2 model))

                                                                if (uncovered model) = 2 then dispatch (AlleZudecken (coverFields model))
                                                                    
                                                            )
                                                        
                                                    ]])
                                               
                                                
                                            
                                        ])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     





                        ]
                ]
            p [] [ str (sprintf "%A" model.Feld)]
                 
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
