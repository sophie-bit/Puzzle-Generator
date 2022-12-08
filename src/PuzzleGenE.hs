module PuzzleGenE where

import Test.QuickCheck

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay
import SMCDEL.Explicit.S5 ( KripkeModelS5(..) )
-- import Data.List (groupBy, elemIndex)
-- import Control.Exception.Base (tryJust)



newtype PuzzleKrMS5 = Puzzle2 KripkeModelS5 deriving (Eq,Ord,Show)

instance Arbitrary PuzzleKrMS5 where
  arbitrary = myArbitrary 3

myArbitrary :: Int -> Gen PuzzleKrMS5
myArbitrary n = do
    let numDates = if n `elem` [3,5]
                then n 
                else 3
    let numMD = if numDates == 3
                    then 2
                    else 3

    allMonths <- shuffle [1..12]    
    allDays <- shuffle [13..28]

    let months = take numMD $ map P allMonths   
    let days = take numMD $ map P  allDays
    let myVocabulary = months ++ days
    
    let monthsIdx = [0..numMD-1]
    let daysIdx = [numMD..(2*numMD)-1]

    -- To do: make this an input for function
    posDates <- shuffle [(x,y) | x<-monthsIdx, y<-daysIdx]
    let dates = take numDates posDates
    -- 
    -- let datesM = groupBy (\a b -> fst a == fst b) dates
    -- let datesD = groupBy (\a b -> snd a == snd b) dates

        
    -- To do: not hardcode this
    let datesM = if numDates == 3
                 then[[0,1],[2]]
                 else [[0,1],[2,3],[4]]
    let datesD = if numDates == 3
                 then [[0,2], [1]]
                 else [[0],[1,2],[3,4]]
    
    let worlds = [0..numDates-1]

    val <- mapM (\w -> do 
      let dateM = fst (dates !! w)
      let dateD = snd (dates !! w)
      
      let holder = repeat False
      let dateBoolM = replaceNth dateM True holder
      let dateBoolD = replaceNth dateD True dateBoolM
      
      let myAssignment = zip myVocabulary dateBoolD
      return (w, myAssignment) 
      ) worlds

    let parts = [("Albert", datesM), ("Bernard", datesD)]
    return $ Puzzle2 $ KrMS5 worlds parts val




run :: Int -> IO ()
run n = do
  Puzzle2 k <- (generate (myArbitrary n) :: IO PuzzleKrMS5)
  print k
  disp k
  let newk = k `update` albertDoesNotKnow (vocabOf k)

  print newk
  disp newk

albertDoesNotKnow :: [Prp] -> Form
albertDoesNotKnow ps = Conj [ Neg (K "Albert" (PrpF p)) | p <- ps, p > P 12 ]

bernardDoesNotKnow :: [Prp] -> Form
bernardDoesNotKnow ps = Conj [ Neg (K "Bernard" (PrpF p)) | p <- ps, p < P 13 ]



replaceNth :: Int -> Bool -> [Bool] -> [Bool]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs) =
  if n == 0 
  then newVal:xs
  else x:replaceNth (n-1) newVal xs

-- isTuple :: (a, b) -> Bool
-- isTuple _ = True

-- dateIdx :: [(Int,Int)] ->   [[(Int,Int)]] -> [[Int]]
-- dateIdx _ [] = []
-- dateIdx d (x:xs) = 
--   if isTuple x
--   then dateIdx d x : dateIdx d xs 
--   else elemIndex x d : dateIdx d xs
 
 
  -- do 
  -- result <- tryJust isTuple x
  -- case result of 
  --   Left _ -> dateIdx d x : dateIdx d xs
  --   Right _ -> elemIndex x d : dateIdx d xs




  -- do
  --   result <- tryJust selectDivByZero (evaluate $ 5 `div` 0)
  --   case result of
  --       Left what -> putStrLn $ "Division by " ++ what
  --       Right val -> putStrLn $ "The answer was: " ++ show val

