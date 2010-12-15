import CaesarCipher

main = do
	ciphertext <- (getLine::IO String)
	
	caesar_brute_force ciphertext
