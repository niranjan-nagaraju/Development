
{- Load parameters from this module -}
import LCG_input

{- Get LCG definition from here -}
import LCG

example_run = take 12 (lcg a b m x0)

{- for a=5, b=1, m=7 and x0=2, the sequence should be 
 - [2,4,0,1,6,3,2,4,0,1,6,3]
 -}

{- for a=b=x0=7 and m=10, the sequence is 
 - [7,6,9,0,7,6,9,0,7,6
 -} 
