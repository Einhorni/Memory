module Domain

type Difficulty =
    | Easy
    | Medium
    | Hard
    | NoGame

let saveHighscoreInList highscoreList highscore =
    if highscoreList = [] then
        let newHSList = [highscore]
        newHSList
    else
        let newHSList highscoreList =
            highscoreList @ [highscore]
        newHSList highscoreList
