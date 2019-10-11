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


type Visibility =
    | Visible
    | Invisible

type Matched =
    | Matched
    | Unmatched
    

type Model = {
    Feld: (int * int) list list // visibility * type of picture
    }

type Msg =
    | SwitchImage of (int * int) list list
    | SwitchSecondImage of (int * int) list list
    | Cover of (int*int) list list


let init () : Model * Cmd<Msg> =
    let initialModel = {
        Feld = [[(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]; [(0,0); (0,0); (0,0); (0,0)]] 
        }
    initialModel, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match  msg with
    | SwitchImage newList ->        
        let nextModel = {
            currentModel with
                Feld = newList
                }
        nextModel, Cmd.none
    | SwitchSecondImage newList ->        //prüfen ob bild doppelt
        let nextModel = {
            currentModel with
                Feld = newList
                }
        nextModel, Cmd.none
    | Cover newList ->
        let nextModel = {
            currentModel with
                Feld = newList
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
    | (_) -> Src "images\Unbenannt.png"
    

//####how many images are already set 
//let tuple = 0,3
let howManyImagesOfOneType model rndImage =
    //[[0,5; 0,5; 0,8; 0,5]; [0,3; 0,1; 0,4; 0,1]; [0,5; 0,3; 0,5; 0,3]; [0,7; 0,4; 0,3; 0,4]]
    model.Feld
    |> List.concat
    |> List.filter (fun x -> x = (rndImage, rndImage) || x = (0, rndImage))
    |> List.length


//####how many images of one pair are visible
let howManyImagesOfOnePairOpen model index1 index2 =
    let (visible, image) = model.Feld.[index1].[index2]
    model.Feld
    |> List.concat
    |> List.filter (fun x -> x = (image,image) && x <> (0,0))
    |> List.length


//####set random number(image) while not 2 same numbers(image) --> if 2, then new random number(image)    
let rec setRandomNumberForImages model howManyImagesOfOneType = 
    let rndNumber = r.Next(1,9)
    match (howManyImagesOfOneType model rndNumber) with
    | 2 -> setRandomNumberForImages model (howManyImagesOfOneType) 
    | _ -> (rndNumber, rndNumber)
    

//####build new list for model (switch cover), set images on Click
let listListWithUncoveresImages i1 i2 model =                                                        
    let innereListe = model.Feld.[i1]
    let neueInnereListe =
        innereListe
        |> List.mapi (fun i x ->
            //####set new image to tile
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


//#### which tiles should be covered
let createNewListe model index1 index2 =                                                                
    model.Feld
    |> List.map (fun x ->
        x
        |> List.map (fun y ->
            //herausfinden, welche eines Paares offen sind - wenn die Fktn ausgeführt wird, sind immer 2 offen (if beim Aufruf)

            //gerade in der Liste durchlaufen
            let (visibleList, imageList) = y
            //wenn y das gleiche wie angewählt
            if y = model.Feld.[index1].[index2] then (visibleList, 0) else y
            ))


//####how many uncovered?
let howManyUncovered model =
    model.Feld
    |> List.map (fun x ->
        x
        |> List.filter (fun y ->
            y = (1,1) ||
            y = (2,2) ||
            y = (3,3) ||
            y = (4,4) ||
            y = (5,5) ||
            y = (6,6) ||
            y = (7,7) ||
            y = (8,8) 
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
                                                        
                                                        yield showImages model.Feld.[index1].[index2] //visibility and value

                                                        if
                                                            tuple <> (1,0) || //warum kann man trotzdem klicken?
                                                            tuple <> (2,0) ||
                                                            tuple <> (3,0) ||
                                                            tuple <> (4,0) ||
                                                            tuple <> (5,0) ||
                                                            tuple <> (6,0) ||
                                                            tuple <> (7,0) ||
                                                            tuple <> (8,0) 

                                                        then

                                                            yield OnClick (fun _ ->
                                                                //dispatch (SwitchImage (newListListrandomImages index1 index2 model))

                                                                match ((howManyUncovered model), (howManyImagesOfOnePairOpen model index1 index2)) with
                                                                | (1,_) -> dispatch (SwitchImage (listListWithUncoveresImages index1 index2 model))
                                                                    //bei 1,0 muss anstatt siwtch image ein anderes switch image rein, in dem geprüft wird, ob es 2 gleiche gibt
                                                                    //diese müssen dann dort auf x,0 gesetzt werden
                                                                | (0,_) -> dispatch (SwitchImage (listListWithUncoveresImages index1 index2 model))
                                                                | (2,2) -> dispatch (Cover (coverFields (createNewListe model index1 index2) )) //wenn ich obiges gemacht habe, weiß ich nicht, ob ich das noch brauche
                                                                | (2,_) -> dispatch (Cover (coverFields model.Feld))
                                                                | _ -> ()
                                                                    
                                                                )
                                                        
                                                    ]
                                                p [] [ str (sprintf "%A: Anzahl Bilder ein Paar" (howManyImagesOfOnePairOpen model index1 index2))]
                                                p [] [ str (sprintf "%A: Anzahl offen" (howManyUncovered model))]
                                                p [] [ str (sprintf "%A: vor Liste zudecken" (createNewListe model index1 index2))]
                                                p [] [ str (sprintf "%A: Produkt Liste zudecken" (coverFields (createNewListe model index1 index2)))]
                                                    ])
                                               
                                                
                                            
                                        ])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     





                        ]
                ]
            p [] [ str (sprintf "%A : Modell" model.Feld)]
            p [] [ str (sprintf "%A : geändertes modell, wenn keine Paare" (coverFields model.Feld))]
                 
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
