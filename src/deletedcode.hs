-- instance Arbitrary PuzzleKrMS5 where
--   arbitrary = myArbitrary

updatesK :: KripkeModelS5 -> [[[Int]]]
updatesK k = do
  ups <- chooseInt(0,7)
  if ups == 0
    then do
        let a = k `update` Conj [albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k)]
        let bK = a `update` bernardKnows (vocabOf a)
        let aK = bK `update` albertKnows (vocabOf bK)
        let sol = map (map fromEnum . truthsInAt aK) (worldsOf aK)
        return sol
    else if ups == 1
      then do 
        let a = k `update` Conj [albertDoesNotKnow (vocabOf k), albertKwBernard (vocabOf k)]
        let bK = a `update` bernardKnows (vocabOf a)
        let aK = bK `update` albertKnows (vocabOf bK)
        let sol = map (map fromEnum . truthsInAt aK) (worldsOf aK)
        return sol
    else if ups == 2
      then do 
        let a = k `update` Conj [albertDoesNotKnow (vocabOf k), albertKwBernard (vocabOf k)]
        let b = a `update` bernardDoesNotKnow (vocabOf a)
        let aK = b `update` albertKnows (vocabOf b)
        let bK = aK `update` bernardKnows (vocabOf aK)  
        let sol = map (map fromEnum . truthsInAt bK) (worldsOf bK)
        return sol
    else if ups == 3
      then do
        let a = k `update` Conj [albertDoesNotKnow (vocabOf k), albertKwBernard (vocabOf k)]
        let b = a `update` Conj [bernardDoesNotKnow (vocabOf a), bernardKwAlbert (vocabOf a)]
        let aK = b `update` albertKnows (vocabOf b)
        let bK = aK `update` bernardKnows (vocabOf aK)  
        let sol = map (map fromEnum . truthsInAt bK) (worldsOf bK)
        return sol
    else if ups == 4
      then do
          let a = k `update` Conj [albertDoesNotKnow (vocabOf k), bernardDoesNotKnow (vocabOf k)]
          let aK = a `update` albertKnows (vocabOf a)
          let bK = aK `update` bernardKnows (vocabOf aK)  
          let sol = map (map fromEnum . truthsInAt bK) (worldsOf bK)
          return sol
      else if ups == 5
        then do 
          let a = k `update` Conj [bernardDoesNotKnow (vocabOf k), bernardKwAlbert (vocabOf k)]
          let aK = a `update` albertKnows (vocabOf a)
          let bK = aK `update` bernardKnows (vocabOf aK)  
          let sol = map (map fromEnum . truthsInAt bK) (worldsOf bK)
          return sol  
      else if ups == 6
        then do 
          let a = k `update` Conj [bernardDoesNotKnow (vocabOf k), bernardKwAlbert (vocabOf k)]
          let b = a `update` albertDoesNotKnow (vocabOf a)
          let bK = b `update` bernardKnows (vocabOf b)
          let aK = bK `update` albertKnows (vocabOf bK)
          let sol = map (map fromEnum . truthsInAt aK) (worldsOf aK)
          return sol
      else do
          let a = k `update` Conj [bernardDoesNotKnow (vocabOf k), bernardKwAlbert (vocabOf k)]
          let b = a `update` Conj [albertDoesNotKnow (vocabOf a), albertKwBernard (vocabOf a)]
          let bK = b `update` bernardKnows (vocabOf b)
          let aK = bK `update` albertKnows (vocabOf bK)
          let sol = map (map fromEnum . truthsInAt aK) (worldsOf aK)
          return sol


end :: KripkeModelS5 -> Int -> KripkeModelS5
end a n = do 
  if n == 1
    then do 
      let bK = a `update` bernardKnows (vocabOf a)
      let aK = bK `update` albertKnows (vocabOf bK)    
      return aK
  else do 
    let aK = a `update` albertKnows (vocabOf a)
    let bK = aK `update` bernardKnows (vocabOf aK)  
    return bK