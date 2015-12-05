-- Igor de Almeida Malheiros Barbosa
-- 115105300
-- Functional Programming I
-- Assignment #4

-- **************************************************************

-- Get the positions in list xs where head of xs is equal to y
positions':: Eq a => a -> [a] -> Int -> [Int]
positions' _ [] _ = []
positions' x (x':xs) acc 
	|(x == x') = acc : positions' x xs (acc+1)
	|otherwise = positions' x xs (acc+1)

-- The list of positions at which item 'x' occurs in list 'xs'
positions:: Eq a => a -> [a] -> [Int]
positions x xs = positions' x xs 1

-- An infinite list [a1,a2,a3,...] of approximations whitch
-- converge to the square root of 'n'
approx::Float -> [Float]
approx n =  1 : map(\n1 -> (n1 + n/n1)/2.0 ) (approx n)

-- Compare if the difference between two consecutives elements
-- in the list is less than 0.0001
cmp::[Float] -> Float
cmp (x1:x2:xs)
	| ( abs (x1-x2) < 0.0001) = x2
	|otherwise = cmp (xs)

-- An estimate of the square root of 'n (n >= 0)
squareRoot:: Float -> Float
squareRoot n = cmp (approx n)

-- Check if n is a prime number
isPrime:: Int -> [Int] -> Bool
isPrime n fact = null [x | x <- takeWhile(\y -> y*y <= n) fact, mod n x == 0]

-- The infinite list of prime numbers
primes:: [Int]
primes = 2 : [n | n <- [3,5..], isPrime n primes]