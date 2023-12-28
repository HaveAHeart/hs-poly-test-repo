{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  initMat :: Int -> Int -> mx
  width :: mx -> Int
  height :: mx -> Int
  getAt :: mx -> Int -> Int -> Int
  setAt :: mx -> Int -> Int -> Int -> mx
  getRow :: mx -> Int -> [Int]
  setRow :: mx -> Int -> [Int] -> mx
  getCol :: mx -> Int -> [Int]
  setCol :: mx -> Int -> [Int] -> mx
  dropRowAndCol :: mx -> Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  initMat _ _ = 0
  width _ = 1
  height _ = 1
    
  getAt mx _ _ = mx
  setAt _ _ _ val = val
  
  getRow mx _ = [mx]
  setRow _ _ row = head row
  
  getCol mx _ = [mx]
  setCol _ _ row = head row
  
  dropRowAndCol _ _ _ = 0

instance Matrix [[Int]] where
  initMat w h = zeroMat where
      zeroMat = [zeroRow | _ <- [0..(h-1)]]
      zeroRow = [0 | _ <- [0..(w-1)]]
  width mx = length (head mx)
  height mx = length mx
  
  getAt mx row col = mx!!row!!col
  setAt mx row col val = setRow mx row rowData where
    setValInRow = \oldRowData colVal ->
      let (headRow, tailRow) = Prelude.splitAt col oldRowData in headRow ++ [colVal] ++ tail tailRow
    rowData = setValInRow (mx!!row) val

  getRow mx row = mx!!row
  setRow mx row rowData = let (headMx, tailMx) = Prelude.splitAt row mx in headMx ++ [rowData] ++ tail tailMx
  
  getCol mx col = (!!col) `fmap` mx
  setCol mx col colData = Prelude.foldl setFunc mx indexedColVal where
    setFunc = \resMx dataPair -> setAt resMx (fst dataPair) col (snd dataPair)
    indexedColVal = [0..col] `zip` colData
    
  dropRowAndCol mx row col = Prelude.foldl modifyRows [] droppedRowMx where
    modifyRows = \resMx rowData -> resMx ++ [dropColVal rowData]
    dropColVal = \rowData -> let (headRow, tailRow) = Prelude.splitAt col rowData in headRow ++ tail tailRow
    droppedRowMx = let (headMx, tailMx) = Prelude.splitAt row mx in headMx ++ tail tailMx


instance Matrix (SparseMatrix Int) where
  initMat w h = SparseMatrix { sparseMatrixWidth = w, sparseMatrixHeight = h, sparseMatrixElements = empty }
  width mx = sparseMatrixWidth mx
  height mx = sparseMatrixHeight mx
  
  getAt mx row col = findWithDefault 0 (row, col) (sparseMatrixElements mx)
  setAt (SparseMatrix w h mx) row col val = setSparseMat w h newMap where
    newMap = Data.Map.filter (/= 0) (insert (row, col) val mx)
    setSparseMat = \wMat hMat mMat -> SparseMatrix { sparseMatrixWidth = wMat, sparseMatrixHeight = hMat, sparseMatrixElements = mMat }
    
  getRow mx row = [findWithDefault 0 (row, i) rowMap | i <- [0..(width mx - 1)]] where
    rowMap = filterWithKey (\k _ -> fst k == row) (sparseMatrixElements mx)
  setRow mx row rowData = Prelude.foldl setFunc mx indexedRowVal where
    setFunc = \resMx dataPair -> uncurry (setAt resMx row) dataPair
    indexedRowVal = [0..(width mx - 1)] `zip` rowData
    
  getCol mx col = [findWithDefault 0 (i, col) colMap | i <- [0..(height mx - 1)]] where
    colMap = filterWithKey (\k _ -> snd k == col) (sparseMatrixElements mx)
  setCol mx col colData = Prelude.foldl setFunc mx indexedColVal where
    setFunc = \resMx dataPair -> setAt resMx (fst dataPair) col (snd dataPair)
    indexedColVal = [0..(height mx - 1)] `zip` colData
    
  dropRowAndCol mx row col = setSparseMat newWidth newHeight updatedMap where
    setSparseMat = \wMat hMat mMat -> SparseMatrix { sparseMatrixWidth = wMat, sparseMatrixHeight = hMat, sparseMatrixElements = mMat }
    newWidth = width mx - 1
    newHeight = height mx - 1
    filteredMap = filterWithKey (\k _ -> (fst k /= row) && (snd k /= col)) (sparseMatrixElements mx)
    updatedMap = correctIndexes `mapKeys` filteredMap
    correctIndexes = \pair -> (if fst pair > row then fst pair - 1 else fst pair, if snd pair > col then snd pair - 1 else snd pair)


-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = Prelude.foldl setDiagVal startMat positions where
  startMat = initMat w w
  positions = [0..(w - 1)] `zip` [0..(w - 1)]
  setDiagVal = \mat pos -> uncurry (setAt mat) pos 1
  
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = initMat

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix mx1 mx2 = Prelude.foldl multFunc zeroMx indexes where
  multFunc = \resMx index -> uncurry (setAt resMx) index (multiply index)
  multiply = Prelude.foldl (\res pair -> res + uncurry (*) pair) 0 . zipRowAndCol
  zipRowAndCol = \index -> getRow mx1 (fst index) `zip` getCol mx2 (snd index)
  zeroMx = zero (width mx2) (height mx1)
  indexes = [0..(height mx1 - 1)] >>= (\row -> [(row, col) | col <- [0..(width mx2 - 1)]])
  
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx
  | width mx == 1 = getAt mx 0 0
  | width mx == 2 = (getAt mx 0 0 * getAt mx 1 1) - (getAt mx 0 1 * getAt mx 1 0)
  | otherwise = Prelude.foldl calcDeterminant 0 signedRowWithIndex
  where
     signedRowWithIndex = [(0, i) | i <- [0..(width mx - 1)]] `zip` signedRow
     signedRow = (\pair -> (-1)^(fst pair) * snd pair) `Prelude.map` ([0..] `zip` getRow mx 0)
     calcDeterminant = \res (index, rowVal) -> res + (rowVal * determinant (getMinor index))
     getMinor = uncurry (dropRowAndCol mx)
    

