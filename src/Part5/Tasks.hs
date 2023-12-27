module Part5.Tasks where

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl func initVal list = myFoldl func initVal' list' where
  initVal' = func initVal (head list)
  list' = tail list

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr func initVal list = myFoldr func initVal' list' where
  initVal' = func (last list) initVal
  list' = init list
   

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле
myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr func [] where
  func = \x res -> [f x] ++ res

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr func [] where
  func = \x res -> f x ++ res

myConcat :: [[a]] -> [a]
myConcat = myFoldr func [] where
  func = (++)

myReverse :: [a] -> [a]
myReverse = foldr func [] where
  func = \x res -> res ++ [x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter boolPred = foldr func [] where
  func = \x res -> getXorEmpty x (boolPred x) ++ res
  getXorEmpty = \x boolVal -> [x | boolVal] 

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition boolPred = foldr func ([], []) where
  func = \x res -> (getXOrEmpty x (boolPred x) ++ fst res, getEmptyOrX x (boolPred x) ++ snd res) 
  getXOrEmpty = \x boolVal -> [x | boolVal] 
  getEmptyOrX = \x boolVal -> [x | not boolVal] 

