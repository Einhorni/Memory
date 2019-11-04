module Helper

open Fetch.Types
open Thoth.Json
open Fetch

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
