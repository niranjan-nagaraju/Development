import IO
import Char

caesar_encrypt_letter :: Int -> Char -> Char
caesar_encrypt_letter key letter =
	if isAlpha(letter)
		then chr(((ord(toUpper(letter)) - ord('A') + key) `mod` 26) + ord('A'))
		else letter


caesar_decrypt_letter :: Int -> Char -> Char
caesar_decrypt_letter key letter =
	if isAlpha(letter)
		then chr(((ord(toUpper(letter)) - ord('A') - key) `mod` 26) + ord('A'))
		else letter


caesar_encrypt :: String -> Int -> String
caesar_encrypt plaintext key = 
	map (caesar_encrypt_letter key) plaintext 


caesar_decrypt :: String -> Int -> String
caesar_decrypt ciphertext key = 
	map (caesar_decrypt_letter key) ciphertext 


caesar_brute_force :: String -> IO()
caesar_brute_force ciphertext =
	mapM_ (\x -> putStrLn ((show x) ++ "\t" ++ caesar_decrypt ciphertext x)) [1 .. 25]


main = do
	let plaintext = "Meet me after the toga party"
	let ciphertext = (caesar_encrypt plaintext 3)
	putStrLn ("Plaintext: " ++ plaintext)
	putStrLn ("Ciphertext: " ++ ciphertext)

	putStrLn ("\nBrute forcing Ciphertext " ++ ciphertext)
	
	caesar_brute_force (caesar_encrypt "Meet me after the toga party" 3)
