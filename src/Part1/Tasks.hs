module Part1.Tasks where

import Data.List (sort)

-- вспомогательные функции для синуса/косинуса
fact :: Int -> Int
fact n =
  if n == 0 then 1
  else n * fact (n-1)

divideByFact :: Double -> Int -> Double
divideByFact x 0 = x
divideByFact x 1 = x
divideByFact x n = divideByFact (x / fromIntegral n) (n - 1)

normAroundRad :: Double -> Double -> Double
normAroundRad x normAround = x - (2 * pi * (fromInteger . floor $ (x + pi - normAround) / (2 * pi)))

pow :: Double -> Int -> Double
pow x n = product (replicate n x)

taylorizeSin :: Double -> Int -> Double
taylorizeSin x k = (sign * angle) `divideByFact` factBase where
  sign = (-1) ^ k
  angle = (x `normAroundRad` pi) ^ (2 * k + 1)
  factBase = 2 * k + 1

taylorizeCos :: Double -> Int -> Double
taylorizeCos x k = (sign * angle) `divideByFact` factBase where
  sign = (-1) ^ k
  angle = (x `normAroundRad` 0) ^ (2 * k)
  factBase = 2 * k


-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum (taylorizeSin x `map` [0..10])


-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum (taylorizeCos x `map` [0..10])


-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD aIn bIn =
  if a == b || (min a b == 0) then max a b
  else myGCD (max a b - min a b) (min a b)
  where
    a = abs aIn
    b = abs bIn

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
getFebDaysByYear :: Integer -> Integer
getFebDaysByYear year =
  if ((year `mod` 4) == 0) && (((year `mod` 100) /= 0) || ((year `mod` 400) == 0)) then 29
  else 28

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | month `elem` [1,3,5,7,8,10,12] && day `elem` [1..31] = True
  | month `elem` [4,6,9,11] && day `elem` [1..30] = True
  | month == 2 && day `elem` [1..(getFebDaysByYear year)] = True
  | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n = product (replicate (fromIntegral n) x)

-- является ли данное число простым?
getSqrt :: Integer -> Integer
getSqrt = floor . sqrt . fromIntegral


isPrime :: Integer -> Bool
isPrime n = 0 `notElem` ([n `mod` i | i <- [2..border]])
  where border = getSqrt n

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат

shiftLeft :: [Double] -> [Double]
shiftLeft points = tail points ++ points

shapeArea :: [Point2D] -> Double
shapeArea points = abs (sumByShiftedY - sumByShiftedX)/ 2  where
  xList = fst `map` points
  yList = snd `map` points -- ну почему не fst/lst :C
  xShifted = shiftLeft xList
  yShifted = shiftLeft yList
  sumByShiftedY = sum (zipWith (*) xList yShifted)
  sumByShiftedX = sum (zipWith (*) yList xShifted)


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | maxSide >= (list!!1 + list!!2) = -1
  | angleCos < 0 = 0
  | angleCos > 0 = 1
  | angleCos == 0 = 2
  | otherwise = -1
  where
      list = reverse (sort [a, b, c])
      c' = head list
      b' = list !! 1
      a' = list !! 2
      maxSide = head list
      angleCos = (c' * c' - a' * a' - b' * b') / ((- 2) * a' * b')


-- c2 = a2 + b2 - 2 cos () ab
