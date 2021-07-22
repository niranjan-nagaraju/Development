{- *OLD problem description*
 - Decode a run-length encoded list.
 - Given a run-length code list generated as specified in problem 10. 
 - Construct its uncompressed version.
 -
 - λ> decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
 - "aaaabccaadeeee"
 -} 


import Control.Exception( assert )


decode = concatMap (\x -> replicate (fst x) (snd x)) 

-- alternate implementation
-- using pattern match for tuple (count,item)
-- and recursively concatenate
decode' [] = []
decode' ((count, item) : xs) = (replicate count item) ++ decode' xs

main = do
	putStr $ assert ( (decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]) == "aaaabccaadeeee" ) ""
	putStr $ assert ( (decode' [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]) == "aaaabccaadeeee" ) ""
