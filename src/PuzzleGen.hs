module PuzzleGenE where

import Test.QuickCheck
    -- ( generate, shuffle, Arbitrary(arbitrary), Gen, chooseInt )

import SMCDEL.Language
import SMCDEL.Internal.TexDisplay
import SMCDEL.Explicit.S5 ( KripkeModelS5(..), World, worldsOf)  --world, worldOf)
import Data.List (groupBy, sortBy, sort)
import SMCDEL.Internal.Help (apply)
import Text.Read(readMaybe)

newtype PuzzleKrMS5 = Puzzle KripkeModelS5 deriving (Eq,Ord,Show)

data Difficulty = Easy | Medium | Hard deriving (Eq,Ord,Show)

-- instance Arbitrary PuzzleKrMS5 where
--   arbitrary num = myArbitrary Easy

-- Gen KripkeModel
myArbitrary :: Difficulty -> Gen (PuzzleKrMS5, Int)
myArbitrary diff = do
    num <- chooseInt(0,7)
    mon <- if diff == Easy
            then chooseInt(2,3)
           else if diff == Medium
            then  chooseInt(4,6)
           else chooseInt(7,12)
    day <- if diff == Easy
            then chooseInt(2,3)
           else if diff == Medium
            then  chooseInt(4,6)
           else chooseInt(7,16)
    dat <- if diff == Easy
            then chooseInt(2, mon*day)
           else if diff == Medium
            then  chooseInt(7,mon*day)
           else chooseInt(12, mon*day)

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
    return (( Puzzle $ KrMS5 worlds parts val), num)


run :: Difficulty -> IO ()
run diff = do
  -- Generate KrMS5 with solution
  (k, p, posB, sol, d) <- tryFindSol diff
  -- Dialogue
  intro p posB
  dia d
  disp k
  -- let player guess
  _ <- tryToGuess sol
  putStrLn "Thank you for playing :)"


  -- Find KrMS5 with solution

tryFindSol :: Difficulty -> IO (KripkeModelS5, Int, [[Int]], [[Int]], [Dialogue])
tryFindSol diff = do
    -- Generate random KrMS5
    (Puzzle k, num) <- generate (myArbitrary diff)
    -- Possible birthdays 
    let posB = map (map fromEnum . truthsInAt k) (worldsOf k)
    let p = length posB

    -- let nK = if p < 7 
    --           then kups k [UABKn, UBK, UAK]
    --          else kups k [UAKB, UBK, UAK]

    -- let num = getNum diff
    let (u, d) = genUD num

    let nK = kups k u
     -- Check if there is 1 solution
    let sol = map (map fromEnum . truthsInAt nK) (worldsOf nK)
    if length sol == 1
      then return (k, p, posB, sol, d)
    else tryFindSol diff


-- Dialogue 

intro :: Int -> [[Int]] -> IO ()
intro p v = do
  putStrLn "Cheryl's Birthday"
  putStrLn $ "\nAlbert and Bernard just became friends with Cheryl, and they want to know when her birthday is. Cheryl gives them a list of " ++ show p ++ " possible dates for her birthday:"
  putStrLn $ unwords . map show $ sort v
  putStrLn "\nCheryl then tells only to Albert the month of her birthday, and tells only to Bernard the day of her birthday. (And Albert and Bernard are aware that she did so.) Albert and Bernard now have the following conversation:\n"

data Dialogue = AKn | BKn | AKB | BKA | BAK | ABK  deriving (Eq,Ord,Show)

mono :: Dialogue -> IO ()
mono a = putStrLn $ case a of
  AKn -> "Albert: “I don't know when Cheryl's birthday is.”"
  BKn -> "Bernard “I don't know when Cheryl's birthday is.”"
  AKB -> "Albert: “I don't know when Cheryl's birthday is, but I know that you don't know either.”"
  BKA -> "Bernard: “I don't know when Cheryl's birthday is, but I know that you don't know either.”"
  BAK -> "Bernard: “At first I didn't know when Cheryl's birthday is, but now I do!”\nAlbert: “Now I know when Cheryl's birthday is.”"
  ABK -> "Albert: “At first I didn't know when Cheryl's birthday is, but now I do!”\nBernard: “Now I know when Cheryl's birthday is.”"

dia :: [Dialogue] -> IO()
dia [] = return()
dia (x:xs) = do mono x
                dia xs


-- Excute updates

data Updates = UABKn | UAKn | UBKn | UAKB | UBKA | UAK | UBK deriving (Eq,Ord,Show)

kup :: KripkeModelS5 -> Updates -> KripkeModelS5
kup k a  = case a of
    UABKn -> k `update` Conj [albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k)]
    UAKn -> k `update` albertDoesNotKnow (vocabOf k)
    UBKn -> k `update` bernardDoesNotKnow (vocabOf k)
    UAKB -> k `update` Conj [albertDoesNotKnow (vocabOf k), albertKwBernard (vocabOf k)]
    UBKA -> k `update` Conj [bernardDoesNotKnow (vocabOf k), bernardKwAlbert (vocabOf k)]
    UAK -> k `update` albertKnows (vocabOf k)
    UBK -> k `update` bernardKnows (vocabOf k)

kups :: KripkeModelS5 -> [Updates] -> KripkeModelS5
kups k [] = k
kups k (x:xs) = do let n = kup k x
                   kups n xs

genUD :: Int -> ([Updates], [Dialogue])
genUD num  = case num of 
            0 -> ([UABKn, UBK, UAK], [AKn, BAK])
            1 -> ([UBKn, UAK, UBK], [BKn, ABK])
            2 -> ([UAKB, UBK, UAK], [AKB, BAK])
            3 -> ([UBKA, UAK, UBK], [BKA, ABK])
            4 -> ([UAKB, UBKn, UAK, UBK], [AKB, BKn, ABK])
            5 -> ([UAKB, UBKA, UAK, UBK], [AKB, BKA, ABK])
            6 -> ([UBKA, UAKn, UBK, UAK], [BKA, AKn, BAK])
            7 -> ([UBKA, UAKB, UBK, UAK], [BKA, AKB, BAK])
            _ -> ([],[])
  
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
