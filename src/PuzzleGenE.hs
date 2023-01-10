module PuzzleGenE where

import Test.QuickCheck
    -- ( generate, shuffle, Arbitrary(arbitrary), Gen, chooseInt )

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay
import SMCDEL.Explicit.S5 ( KripkeModelS5(..), World, worldsOf)  --world, worldOf)
import Data.List (groupBy, sortBy, sort)
import SMCDEL.Internal.Help (apply)
import Text.Read(readMaybe)

newtype PuzzleKrMS5 = Puzzle2 KripkeModelS5 deriving (Eq,Ord,Show)

-- Gen KripkeModel
myArbitrary :: Gen PuzzleKrMS5
myArbitrary = do
    -- Choose parameters puzzle
    mon <- chooseInt(2,12)
    day <- chooseInt(2, 16)
    dat <- chooseInt(3, mon*day)
    -- Choose specific months
    allMonths <- shuffle [1..12]    
    allDays <- shuffle [13..28]
    let months = take mon $ map P allMonths   
    let days = take day $ map P  allDays
    let myVocabulary = months ++ days

    -- Gen possible birthdays
    posDates <- shuffle [(x,y) | x<-months, y<-days]
    let dates = zip [1..] $ take dat posDates
    let datesM = map (map fst) $ groupBy (\ (_,(m1,_)) (_,(m2,_)) -> m1 == m2) $ sortBy (\ (_,(m1,_)) (_,(m2,_)) -> compare m1 m2) dates
    let datesD = map (map fst) $ groupBy (\ (_,(_,d1)) (_,(_,d2)) -> d1 == d2) $ sortBy (\ (_,(_,d1)) (_,(_,d2)) -> compare d1 d2) dates
    
    let worlds = map fst dates
    let val = map (\(w,(d,m)) ->  (w,[(p, p ==d || p==m) | p <- myVocabulary ])) dates
    let parts = [("Albert", datesM), ("Bernard", datesD)]
    return $ Puzzle2 $ KrMS5 worlds parts val


run :: IO ()
run = do 
  -- Generate KrMS5 with solution
  (k, p, posB, sol) <- tryFindSol
  -- Dialogue
  intro p posB
  if p < 7 
    then dialogue AKn
  else dialogue AKB
  dialogue BK
  dialogue AK
  disp k
  -- let player guess
  _ <- tryToGuess sol
  putStrLn "Thankyou for playing :)"


  -- Find KrMS5 with solution
  
tryFindSol :: IO (KripkeModelS5, Int, [[Int]], [[Int]])
tryFindSol = do
    -- Generate random KrMS5
    Puzzle2 k <- generate myArbitrary
    -- Possible birthdays 
    let posB = map (map fromEnum . truthsInAt k) (worldsOf k)
    let p = length posB 
    
    -- Excute updates
    let aKB = if p < 7
        then k `update` Conj [albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k)]
        else k `update` Conj [albertDoesNotKnow (vocabOf k), albertKwBernard (vocabOf k)]
    let bK = aKB `update` bernardKnows (vocabOf aKB)
    let aK = bK `update` albertKnows (vocabOf bK)    

    -- Check if there is 1 solution
    let sol = map (map fromEnum . truthsInAt aK) (worldsOf aK)
    if length sol == 1
      then return (k, p, posB, sol)
    else tryFindSol 
      

-- Dialogue 

intro :: Int -> [[Int]] -> IO ()
intro p v = do
  putStrLn "Cheryl's Birthday"
  putStrLn $ "\nAlbert and Bernard just became friends with Cheryl, and they want to know when her birthday is. Cheryl gives them a list of " ++ show p ++ " possible dates for her birthday:"
  putStrLn $ unwords . map show $ sort v
  putStrLn "\nCheryl then tells only to Albert the month of her birthday, and tells only to Bernard the day of her birthday. (And Albert and Bernard are aware that she did so.) Albert and Bernard now have the following conversation:\n"

data Dialogue = AKn | AKB | BKA | AK | BK deriving (Eq,Ord,Show)

dialogue :: Dialogue -> IO ()
dialogue a = do  
  if a == AKn
  then putStrLn "Albert: “I don't know when Cheryl's birthday is.”"
  else if a == AKB 
  then putStrLn "Albert: “I don't know when Cheryl's birthday is, but I know that you don't know either.”"
  else if a == BKA 
  then putStrLn "Bernard: “I don't know when Cheryl's birthday is, but I know that you don't know either.”"
  else if a == BK
  then putStrLn "Bernard: “At first I didn't know when Cheryl's birthday is, but now I do!”"
  else if a == AK
  then putStrLn "Albert: “Now I know when Cheryl's birthday is.”"
  else putStrLn "ERROR: Dialogue not found"


-- Guess solution

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
                else do 
                  putStrLn $ "The correct answer is: " ++ show (head s)
                  return False

tryAgain :: IO Bool
tryAgain = do
  putStrLn "Please anwer y/n"
  input <- getLine
  case input of
    "y" -> return True
    "n" -> return False
    _ -> tryAgain

-- Updates

albertKnows :: [Prp] -> Form
albertKnows ps = Conj [ Kw "Albert" (PrpF p) | p <- ps]

bernardKnows :: [Prp] -> Form
bernardKnows ps = Conj [ Kw "Bernard" (PrpF p) | p <- ps]

albertDoesNotKnow :: [Prp] -> Form
albertDoesNotKnow ps = Conj [ Neg (K "Albert" (PrpF p)) | p <- ps, p > P 12 ]

bernardDoesNotKnow :: [Prp] -> Form
bernardDoesNotKnow ps = Conj [ Neg (K "Bernard" (PrpF p)) | p <- ps, p < P 13 ]

albertKwBernard :: [Prp] -> Form
albertKwBernard ps = Conj [K "Albert" $ Neg (K "Bernard" (PrpF p)) | p <- ps, p < P 13 ]

bernardKwAlbert :: [Prp] -> Form
bernardKwAlbert ps = Conj [K "Bernard" $ Neg (K "Albert" (PrpF p)) | p <- ps, p > P 12 ]


-- Get validations
truthsInAt :: KripkeModelS5 -> World -> [Prp]
truthsInAt m@(KrMS5 _ _ val) w = filter (apply $ apply val w)(vocabOf m)
