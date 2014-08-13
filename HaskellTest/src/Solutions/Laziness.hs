module Solutions.Laziness where

integersFrom :: Integer -> [Integer]
integersFrom n = n : integersFrom (n + 1)

isPrime :: [Integer] -> Integer -> Bool
isPrime ps n = (all (\ i -> n `mod` i /= 0) . takeWhile (\ i -> i * i < n)) ps

-- sito Erastotenesa
primes2 :: [Integer]
primes2 = 2 : filter (isPrime primes2) (integersFrom 3)

