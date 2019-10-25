module DataAccess

//#r @"C:\Users\cas\.nuget\packages\newtonsoft.json\12.0.2\lib\netstandard2.0\Newtonsoft.Json.dll"
open System
open Domain


let saveHighscore dateiname (state: (int*int*DateTime*Difficulty) list) =
    let serializedHighscores =
        Newtonsoft.Json.JsonConvert.SerializeObject(state)
    System.IO.File.WriteAllText(dateiname,serializedHighscores)

let callHighscores dateiname =
    if System.IO.File.Exists(dateiname) then 
        let inhalt = System.IO.File.ReadAllText(dateiname)
        let highscores =
            Newtonsoft.Json.JsonConvert.DeserializeObject<(int*int*DateTime*Difficulty) list>(inhalt)
        highscores
    else 
        failwith "Noch kein Highscore vorhanden" 
         