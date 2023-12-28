module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= [[zz, zz, zz, zz, zz], [zz, zz, zz, zz, zz], [zz, zz, zz, zz, zz], [zz, zz, zz, zz, zz], [zz, zz, zz, zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_dropRowAndCol = do
    dropRowAndCol lst1 0 1 @?= lstRes
    dropRowAndCol sp1 0 1 @?= spRes
    where
        lst1 = [[5 :: Int, 2 :: Int], [4 :: Int, 0 :: Int]]
        lstRes = [[4 :: Int]]
        sp1 = SparseMatrix 2 4 (Data.Map.fromList [((1, 0), 15 :: Int), ((3, 1), 18 :: Int)])
        spRes = SparseMatrix 1 3 (Data.Map.fromList [((0, 0), 15 :: Int)])

unit_multiply = do
    multiplyMatrix v1 v2 @?= 4
    multiplyMatrix lst1 lst2 @?= lstRes
    multiplyMatrix sp1 spEye @?= spRes
    where
        v1 = 1 :: Int
        v2 = 4 :: Int
        lst1 = [[4 :: Int, 8 :: Int], [11 :: Int, 7 :: Int], [3 :: Int, 1 :: Int]]
        lst2 = [[-2 :: Int, 0 :: Int, 1 :: Int], [3 :: Int, 17 :: Int, 10 :: Int]]
        lstRes = [[16 :: Int, 136 :: Int, 84 :: Int], [-1 :: Int, 119 :: Int, 81 :: Int], [-3 :: Int, 17 :: Int, 13 :: Int]]
        sp1 = SparseMatrix 2 4 (Data.Map.fromList [((1, 0), 15 :: Int), ((3, 1), 18 :: Int)])
        spEye = SparseMatrix 2 2 (Data.Map.fromList [((0, 0), 1 :: Int), ((1, 1), 1 :: Int)])
        spRes = SparseMatrix 2 4 (Data.Map.fromList [((1, 0), 15 :: Int), ((3, 1), 18 :: Int)])
        
unit_determinant = do
    determinant v1 @?= v1
    determinant lst1 @?= lst1Det
    determinant lstWithZeroDet @?= zz
    determinant lst3_3 @?= lst3_3Det
    determinant sp1 @?= spDet
    where
        v1 = 5 :: Int
        lst1 = [[1 :: Int, 2:: Int], [3 :: Int, 4 :: Int]]
        lst1Det = (-2) :: Int
        lstWithZeroDet = [[1 :: Int, 2:: Int], [1 :: Int, 2:: Int]]
        zz = 0 :: Int
        lst3_3 = [[1 :: Int, 2 :: Int, 3 :: Int], [5 :: Int, 5 :: Int, 5 :: Int], [7 :: Int, -2 :: Int, 9 :: Int]]
        lst3_3Det = (-100) :: Int
        sp1 = SparseMatrix 3 3 (Data.Map.fromList [
            ((0, 0), 11 :: Int), ((0, 2), 2 :: Int), 
            ((1, 1), 2 :: Int), ((1, 2), 15 :: Int),
            ((2, 0), 7 :: Int)])
        spDet = -28