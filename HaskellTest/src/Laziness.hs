{-# LANGUAGE RankNTypes #-}
module Laziness where

import System.IO.Unsafe

-- NIE RÓBCIE TEGO W DOMU!!!
computeLoudly :: forall b . String -> b -> b
computeLoudly info = seq (let unsafeLog = unsafePerformIO $ putStrLn info in unsafeLog)  
  
unsafeFib :: [Integer]
unsafeFib = 0 : 1 : unsafeFib' 0 1
  
unsafeFib' :: (Num t, Show t) => t -> t -> [t]
unsafeFib' p1 p2 = computeLoudly (" -- " ++ show next) (next : unsafeFib' p2 next) 
  where next = p1 + p2

rotation :: [a] -> [a]
rotation lst = lst ++ rotation lst

fibonacci2 :: [Integer]
fibonacci2 = 0 : 1 : zipWith (+) fibonacci2 (tail fibonacci2)

-- Ćwiczenie: sito Erastotenesa