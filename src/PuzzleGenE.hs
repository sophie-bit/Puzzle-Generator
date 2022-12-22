module PuzzleGenE where

import Test.QuickCheck

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay
import SMCDEL.Explicit.S5 ( KripkeModelS5(..), World, worldsOf)  --world, worldOf)
import Data.List (groupBy, sortBy)
import SMCDEL.Internal.Help (apply)
import Text.Read(readMaybe)


newtype PuzzleKrMS5 = Puzzle2 KripkeModelS5 deriving (Eq,Ord,Show)

instance Arbitrary PuzzleKrMS5 where
  arbitrary = myArbitrary 2 2 3

myArbitrary :: Int -> Int -> Int -> Gen PuzzleKrMS5
myArbitrary mon day dat = do
    allMonths <- shuffle [1..12]    
    allDays <- shuffle [13..28]
    let months = take mon $ map P allMonths   
    let days = take day $ map P  allDays
    let myVocabulary = months ++ days

    -- let posDates = zip [1..] [(x,y) | x<-months, y<-days]
    -- let dates = if n ==3
    --             then map (posDates !!) [0,1,2]
    --             else map (posDates !!) [0,1,4,5,8]

    posDates <- shuffle [(x,y) | x<-months, y<-days]
    let dates = zip [1..] $ take dat posDates
    let datesM = map (map fst) $ groupBy (\ (_,(m1,_)) (_,(m2,_)) -> m1 == m2) $ sortBy (\ (_,(m1,_)) (_,(m2,_)) -> compare m1 m2) dates
    let datesD = map (map fst) $ groupBy (\ (_,(_,d1)) (_,(_,d2)) -> d1 == d2) $ sortBy (\ (_,(_,d1)) (_,(_,d2)) -> compare d1 d2) dates
    
    let worlds = map fst dates
    let val = map (\(w,(d,m)) ->  (w,[(p, p ==d || p==m) | p <- myVocabulary ])) dates
    let parts = [("Albert", datesM), ("Bernard", datesD)]
    return $ Puzzle2 $ KrMS5 worlds parts val


run :: Int -> Int -> Int -> IO ()
run m d p = do
  Puzzle2 k <- (generate (myArbitrary m d p) :: IO PuzzleKrMS5)
  let val = map (map fromEnum . truthsInAt k) (worldsOf k)
  let abKn = k `update` Conj [ albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k) ]
  let bK = abKn `update` bernardDoesKnow (vocabOf abKn)
  let aK = bK `update` bernardDoesKnow (vocabOf bK)
  let newval = map (map fromEnum . truthsInAt bK) (worldsOf bK)
  if length newval /= 1
    then putStrLn "ERROR: There is no solution"
  else do 
    intro p val
    disp k
    dialogue "abKn"
    dialogue "aK"
    _ <- tryToGuess newval
    disp aK


albertDoesNotKnow :: [Prp] -> Form
albertDoesNotKnow ps = Conj [ Neg (K "Albert" (PrpF p)) | p <- ps, p > P 12 ]

bernardDoesNotKnow :: [Prp] -> Form
bernardDoesNotKnow ps = Conj [ Neg (K "Bernard" (PrpF p)) | p <- ps, p < P 13 ]

-- albertKnowsBernardNot :: [Prp] -> Form
-- albertKnowsBernardNot ps = Conj [ Kw (Neg (K "Bernard" (PrpF p))) | p <- ps, p < P 13 ]

albertDoesKnow :: [Prp] -> Form
albertDoesKnow ps = Conj [ Kw "Albert" (PrpF p) | p <- ps]

bernardDoesKnow :: [Prp] -> Form
bernardDoesKnow ps = Conj [ Kw "Bernard" (PrpF p) | p <- ps]

truthsInAt :: KripkeModelS5 -> World -> [Prp]
truthsInAt m@(KrMS5 _ _ val) w = filter (apply $ apply val w)(vocabOf m)

intro :: Int -> [[Int]] -> IO ()
intro p v = do
  putStrLn "Cheryl's Birthday"
  putStrLn $ "\nAlbert and Bernard just became friends with Cheryl, and they want to know when her birthday is. Cheryl gives them a list of " ++ show p ++ " possible dates for her birthday:"
  putStrLn $ unwords . map show $ v
  putStrLn "Cheryl then tells only to Albert the month of her birthday, and tells only to Bernard the day of her birthday. (And Albert and Bernard are aware that she did so.) Albert and Bernard now have the following conversation:\n"

dialogue :: String -> IO ()
dialogue a = do 
  if a == "abKn"
  then putStrLn "Albert: “I don't know when Cheryl's birthday is.”\nBernard: “At first I didn't know when Cheryl's birthday is, but now I do!”"
  else if a == "bK"
  then putStrLn "Bernard: “Now I also know when Cheryl's birthday is.””"
  else if a == "aK"
  then putStrLn "Albert: “Now I also know when Cheryl's birthday is.”"
  else putStrLn "ERROR: Dialogue not found"

tryToGuess :: [[Int]] -> IO Bool
tryToGuess s = do
  putStrLn "\nWhen is Cheryl's birthday?"
  input <- getLine
  case readMaybe input of 
    Nothing -> do 
      putStrLn "Please use the following format: [m,d]"
      tryToGuess s
    Just n -> do
      if n == head s
        then do putStrLn "Correct!"
                return True
        else do putStrLn "Incorrect. Do you want to try again?"
                a <- tryAgain
                if a 
                then tryToGuess s
                else return False

tryAgain :: IO Bool
tryAgain = do 
  putStrLn "Please anwer 'y'/'n'"
  input <- getLine
  let str = "'"++input++"'"
  case readMaybe str of 
    Nothing -> do 
      tryAgain
    Just n -> do
      if n == 'y'
      then return True
      else if n == 'n'
      then return False 
      else tryAgain
  
