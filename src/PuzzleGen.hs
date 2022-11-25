module PuzzleGen where

import Test.QuickCheck

import SMCDEL.Language
import SMCDEL.Symbolic.S5

newtype PuzzleKnS = Puzzle KnowStruct deriving (Eq, Show)

instance Arbitrary PuzzleKnS where
  arbitrary = do
    extraDays <- sublistOf $ map P [15..16] -- FIXME: increase?
    let days = [P 13, P 14] ++ extraDays
    extraMonths <- sublistOf $ map P [3..4] -- FIXME: increase?
    let months = [P 1, P 2] ++ extraMonths
    let myVocabulary = months ++ days
    possibilities <- sublistOf [ Conj [PrpF d, PrpF m] | d <- days, m <- months ] -- FIXME not empty!
    let statelaw = Conj
                       [ Disj possibilities
                       , Conj [ Neg $ Conj [PrpF d1, PrpF d2] | d1
                       <- days, d2 <- days, d1 < d2 ]
                       , Conj [ Neg $ Conj [PrpF m1, PrpF m2] | m1
                       <- months, m2 <- months, m1 < m2 ]
                       ]
    -- FIXME: do not use random obs!
    obs <- mapM (\i -> do
      obsVars <- sublistOf myVocabulary
      return (i,obsVars)
      ) ["Albert", "Bernard"]
    return $ Puzzle $ KnS myVocabulary (boolBddOf statelaw) obs
