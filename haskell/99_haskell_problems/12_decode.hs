{- 
 - Decode a run-length encoded list.
 - Given a run-length code list generated as specified in problem 10. 
 - Construct its uncompressed version.
 -} 

decode enc_list =
	concatMap (\x -> replicate (fst x) (snd x)) enc_list


main = do
	print $ decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
