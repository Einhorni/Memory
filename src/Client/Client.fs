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
    | AlleZudecken


let initialCounter () = Fetch.fetchAs<Counter> "/api/init"

let init () : Model * Cmd<Msg> =
    let initialModel = {
        A1 = Invisible, Unmatched
        Feld = [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
        }
    initialModel, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    | Bildwechsel neueListe ->
        
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = neueListe
                }
        nextModel, Cmd.none
    | AlleZudecken ->
        let nextModel = {
            currentModel with
                A1 = Visible, Unmatched
                Feld = [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
                }
        nextModel, Cmd.none
        
let r = Random()

let chooseImages =
    match r.Next(1,9) with
    | 1 -> Src "images\Unbenannt2.png"
    | 2 -> Src "images\Unbenannt3.png"
    | 3 -> Src "images\Unbenannt4.png"
    | 4 -> Src "images\Unbenannt5.png"
    | 5 -> Src "images\Unbenannt6.png"
    | 6 -> Src "images\Unbenannt7.png"
    | 7 -> Src "images\Unbenannt8.png"
    | 8 -> Src "images\Unbenannt9.png"
    | _ -> Src "images\Unbenannt.png"
    
    



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
                            //yield! [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
                            yield! model.Feld
                            |> List.mapi (fun index1 list ->
                                tr [][
                                    yield! list
                                    |> List.mapi (fun index2 intMarker ->
                                        td
                                            []
                                            [
                                                
                                                img
                                                    [
                                                        if model.Feld.[index1].[index2] = (0,0) then
                                                            yield Src "images\Unbenannt.png"
                                                        else yield Src "images\Unbenannt2.png"


                                                        let (showNumber, isNumber) = intMarker


                                                        let neueListListHerstellen i1 i2 =                                                        
                                                            let innereListe = model.Feld.[i1]
                                                            let neueInnereListe =
                                                                innereListe
                                                                |> List.mapi (fun i x ->
                                                                    if i = i2 then
                                                                        
                                                                        (showNumber+1, isNumber+1)
                                                                    else x)
                                                            let neueÄußereListe =
                                                                model.Feld
                                                                |> List.mapi (fun i x -> if i = i1 then neueInnereListe else x)
                                                            neueÄußereListe
                                                        if model.Feld.[index1].[index2] = (0,0) then //damit nicht doppelt gedrückt werden kann
                                                            yield OnClick (fun _ ->
                                                                dispatch (Bildwechsel (neueListListHerstellen index1 index2))
                                                                let howManyNot0 =
                                                                    model.Feld
                                                                    |> List.map (fun x ->
                                                                        x
                                                                        |> List.filter (fun y ->
                                                                            y <> (0,0)))

                                                                        |> List.concat
                                                                    |> List.length
                                                                if howManyNot0 = 2 then dispatch (AlleZudecken)
                                                                    
                                                            )
                                                        
                                                    ]])
                                               
                                                
                                            
                                        ])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     





                        ]
                ]
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
