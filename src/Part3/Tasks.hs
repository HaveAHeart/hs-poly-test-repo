module Part3.Tasks where

import Util (notImplementedYet)
import Data.List
import Data.Char
import Data.Maybe

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f `fmap` [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff = iterate

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)

mostFreq :: [Int] -> Int
mostFreq [] = 0
mostFreq lst = head (grouped!!index) where
  index = fromMaybe 0 (elemIndex (maximum lengths) lengths)
  lengths = length `fmap` grouped
  grouped = group (sort (digitToInt `fmap` intercalate "" (show `fmap` lst)))


-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = nub

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = pairsWithValues where
  uniqResults = nub (f `fmap` l)
  pairsWithIndices = (\res -> (res, findIndices (\currEl -> f currEl == res) l)) `fmap` uniqResults
  pairsWithValues = (\pair -> (fst pair, (l!!) `fmap` (snd pair))) `fmap` pairsWithIndices

