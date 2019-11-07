
module ClientTypes

open System
open Domain

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
    Timer: float * float
    IntervalID: int
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
    | ChangeTimer of DateTime
    | ChangeTimeoutID of int
