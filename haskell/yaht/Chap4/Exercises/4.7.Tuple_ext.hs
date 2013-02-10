{--
 - Extend Tuple from 4.6.
 - one tuple if One tuple
 - pair if Two tuple
 - triplet if Three tuple
 - four tuple if Four tuple
 -}

import Tuple

{- I have no idea how this works -}
fromTuple (One   a      ) = Left  (Left  a	      )
fromTuple (Two   a b    ) = Left  (Right (a, b)   )
fromTuple (Three a b c  ) = Right (Left  (a,b,c)  )
fromTuple (Four  a b c d) = Right (Right (a,b,c,d))

{- My soln: Doesn't work -}
elements (One a       ) = (a)
elements (Two a b     ) = (a,b)
elements (Three a b c ) = (a,b,c)
elements (Four a b c d) = (a,b,c,d)


