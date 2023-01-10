module MuddyChildren where

import Test.QuickCheck

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay
-- import SMCDEL.Explicit.S5 ( KripkeModelS5(..) )
import SMCDEL.Explicit.S5
-- import SMCDEL.Examples.Cheryl (possibilities)
-- import Data.List (groupBy)


newtype PuzzleKrMS5 = Muddy KripkeModelS5 deriving (Eq,Ord,Show)

instance Arbitrary PuzzleKrMS5 where
  arbitrary = myArbitrary 3

myArbitrary :: Int -> Gen PuzzleKrMS5
myArbitrary n = do
    let num_Children = n   
    let myVocabulary = map P [1..num_Children]
    let posVal= allPropositions myVocabulary
    
    
    let worlds = [0..(length posVal - 1)]

    val <- mapM (\w -> do 
      let myAssignment = posVal !! w
      return (w, myAssignment) 
      ) worlds

    let myPartitions = [[[0,4],[1,5],[2,6],[3,7]],[[0,2],[1,3],[4,6],[5,7]],[[0,1],[2,3],[4,5],[6,7]]]

    let parts = zip defaultAgents myPartitions
    return $ Muddy $ KrMS5 worlds parts val

run :: Int -> IO ()
run n = do
  Muddy k <- (generate (myArbitrary n) :: IO PuzzleKrMS5)
  print k
  disp k

allPropositions :: [a] -> [[(a, Bool)]]
allPropositions = foldr (\x xs -> (:) <$> [(x,True),(x,False)] <*> xs) [[]]