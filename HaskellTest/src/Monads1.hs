module Monads1 where

data Country = Country { government :: Maybe Government }

data Government = Government { primeMinister :: Maybe Person }
data Person = Person { firstName :: String, lastName :: String, birthYear :: Maybe Integer }

-- Bardzo brzydki kod:

primeMinisterAge :: Country -> Maybe Integer
primeMinisterAge Country { government = gov } =
  case gov of
    Nothing -> Nothing
    Just g -> 
      case primeMinister g of
        Nothing -> Nothing
        Just Person { birthYear = byear } ->
          case byear of
            Nothing -> Nothing
            Just year -> Just $ 2014 - year
            
med :: Person
med = Person "Dmitrij" "Miedwiediew" (Just 1965)

russianGovernment :: Government
russianGovernment = Government (Just med)

russia :: Country
russia = Country (Just russianGovernment)

-- Ćwiczenie: jak to poprawić?