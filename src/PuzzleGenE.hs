module PuzzleGenE where

import Test.QuickCheck

import SMCDEL.Language 
import SMCDEL.Internal.TexDisplay()
import SMCDEL.Explicit.S5 ( randomPartFor, KripkeModelS5(..) )
import SMCDEL.Symbolic.S5()


newtype PuzzleKrMS5 = Puzzle2 KripkeModelS5 deriving (Eq,Ord,Show)

instance Arbitrary PuzzleKrMS5 where
  arbitrary = do
    let nonActualWorlds =[0,1,2,3]
    let worlds = 0 : nonActualWorlds
    val <- mapM (\w -> do
      myAssignment <- zip defaultVocabulary <$> infiniteListOf (choose (True,False))
      return (w,myAssignment)
      ) worlds
    parts <- mapM (\i -> do
      myPartition <- randomPartFor worlds
      return (i,myPartition)
      ) ["Albert", "Bernard"]
    return $ Puzzle2 $ KrMS5 worlds parts val
  