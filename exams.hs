-- Igor de Almeida Malheiros Barbosa
-- 115105300
-- Functional Programming I
-- Winter Exam 2014

-- **************************************************************

partialSums:: [Int] -> [Int]
partialSums(x:[]) = x:[]
partialSums (x:xs) = x: zipWith (+) (partialSums(x:xs)) (xs)

from:: Int -> [Int]
from n = iterate (+1) (n)

powerLists:: [[Int]]
powerLists = map (\n -> iterate(*n) (1) ) (from 1)

-- For test
--powerLists = map (\n -> take 5 (iterate(*n) (1) )) (from 1)

powerLists':: [[Int]]
powerLists' = [[y^x | x <- [1..]] | y <- [1..]]

-- For test
--powerLists' = [ take 5 ([y^x | x <- [1..]]) | y <- [1..]]

--factorial 0 = 1
--factorial n = n * factorial (n - 1)

facTuples:: [(Int,Int)]
facTuples = iterate ( \(n, fact) -> (n+1, (n+1)*fact) )  (1,1)