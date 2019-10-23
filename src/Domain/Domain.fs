module Domain

open System

type Difficulty =
    | Easy
    | Medium
    | Hard
    | NoGame

//let highscoreList = []
//let highscore = (5,4, DateTime.Now)

let saveHighscoreInList highscoreList highscore =
    if highscoreList = [] then
        let newHSList = [highscore]
        newHSList
    else
        let newHSList highscoreList =
            highscoreList @ [highscore]
        newHSList highscoreList
            
//let newList = saveHighscore highscoreList highscore
 
//saveHighscore newList highscore
