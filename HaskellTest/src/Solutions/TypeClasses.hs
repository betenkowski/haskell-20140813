module Solutions.TypeClasses where

instance Num Bool where
  abs = id
  signum = id
  
  fromInteger 0 = False
  fromInteger _ = True
  
  False + b = b
  True + _  = True
  
  False * _ = False
  True * b = b

