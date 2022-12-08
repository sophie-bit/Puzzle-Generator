module PuzzleGenE where

import Test.QuickCheck

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay
import SMCDEL.Explicit.S5 ( KripkeModelS5(..) )
import Data.List (groupBy, sortBy)
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

    posDates <- shuffle [(x,y) | x<-months, y<-days]
    let dates = zip [1..] $ take numDates posDates

    let datesM = map (map fst) $ groupBy (\ (_,(m1,_)) (_,(m2,_)) -> m1 == m2) $ sortBy (\ (_,(m1,_)) (_,(m2,_)) -> compare m1 m2) dates
    let datesD = map (map fst) $ groupBy (\ (_,(_,d1)) (_,(_,d2)) -> d1 == d2) $ sortBy (\ (_,(_,d1)) (_,(_,d2)) -> compare d1 d2) dates
    
    let worlds = map fst dates

    let val = map (\(w,(d,m)) ->  (w,[(p, p ==d || p==m) | p <- myVocabulary ])) dates

    let parts = [("Albert", datesM), ("Bernard", datesD)]
    return $ Puzzle2 $ KrMS5 worlds parts val


run :: Int -> IO ()
run n = do
  Puzzle2 k <- (generate (myArbitrary n) :: IO PuzzleKrMS5)
  print k
  disp k
  let newk = k `update` Conj [ albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k) ]

  print newk
  disp newk
  
albertDoesNotKnow :: [Prp] -> Form
albertDoesNotKnow ps = Conj [ Neg (K "Albert" (PrpF p)) | p <- ps, p > P 12 ]

bernardDoesNotKnow :: [Prp] -> Form
bernardDoesNotKnow ps = Conj [ Neg (K "Bernard" (PrpF p)) | p <- ps, p < P 13 ]