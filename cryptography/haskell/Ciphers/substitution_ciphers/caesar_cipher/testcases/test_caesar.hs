import CaesarCipher

main = do
	plaintext <- (getLine::IO String)
	key <- (readLn::IO Int)
	
	let ciphertext = (caesar_encrypt plaintext key)
	putStrLn ("Encrypted Ciphertext: " ++ ciphertext)

	let dec_plaintext = (caesar_decrypt ciphertext key)
	putStrLn ("Decrypted Plaintext: " ++ dec_plaintext)

