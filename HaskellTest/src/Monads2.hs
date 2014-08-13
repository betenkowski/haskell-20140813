module Monads2 where

import Monads1

primeMinisterAge' :: Country -> Maybe Integer
primeMinisterAge' c =
  government c >>= \gov ->
  primeMinister gov >>= \pm ->
  birthYear pm >>= \year ->
  Just $ 2014 - year

primeMinisterAge'' :: Country -> Maybe Integer
primeMinisterAge'' c = do
  gov <- government c
  pm <- primeMinister gov
  year <- birthYear pm
  return $ 2014 - year

