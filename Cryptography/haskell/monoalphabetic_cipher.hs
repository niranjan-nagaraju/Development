import IO
import Char

monoalpha_encrypt :: String -> String -> String
monoalpha_encrypt plaintext keytext = 
	 map (\x -> 
			if isAlpha(x)
				then (keytext !! (ord(toUpper(x)) - ord('A')))
				else x
				) plaintext

{-
main = do
	let plaintext = "Meet me after the toga party"
	
	keytext <- getLine
	
	if length(keytext) != 26
		then 

	let ciphertext = (monoalpha_encrypt plaintext 3)
	putStrLn ("Plaintext: " ++ plaintext)
	putStrLn ("Ciphertext: " ++ ciphertext)
	-}
