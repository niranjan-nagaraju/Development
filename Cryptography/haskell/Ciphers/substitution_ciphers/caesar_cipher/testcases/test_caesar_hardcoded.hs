import CaesarCipher

main = do
	let plaintext = "Meet me after the toga party"
	let ciphertext = (caesar_encrypt plaintext 3)
	putStrLn ("Plaintext: " ++ plaintext)
	putStrLn ("Ciphertext: " ++ ciphertext)

	putStrLn ("\nBrute forcing Ciphertext " ++ ciphertext)
	
	caesar_brute_force (caesar_encrypt "Meet me after the toga party" 3)
