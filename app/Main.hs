module Main (main) where
import PuzzleGen

main :: IO ()
main = do
    putStrLn "Welcome to the Cheryl's Birthday puzzle generator!\nDo you want ane Easy, Medium or Hard puzzle?"
    diff <- getDiff 
    run diff
    
getDiff :: IO Difficulty
getDiff = do   
    input <- getLine
    case input of
        "Easy" -> return Easy
        "Medium" -> return Medium
        "Hard" -> return Hard
        _ -> do putStrLn "Please answer: Easy, Medium, Hard"
                getDiff
