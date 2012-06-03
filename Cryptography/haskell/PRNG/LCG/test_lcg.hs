
{- Load parameters from this module -}
import LCG_input

{- Get LCG definition from here -}
import LCG

example_run = take 12 (lcg a b m x0)

{- for a=5, b=1, m=7 and x0=2, the sequence should be 
 - 2, 3, 0, 1, 6, 7, 4, 5, 2, 3, 0, 1 
 -}

{- *Main> example_run 
 - [2,3,0,1,6,7,4,5,2,3,0,1]
 -}
