import MonoalphabeticCipher
import Char

main = do
	plaintext <- (getLine::IO String)
	keystring <- (getLine::IO String)

	let keystring_u = map toUpper keystring
	
	let keymap = keystring_u ++ [x | x <- ['A' .. 'Z'], notElem x keystring_u]
	let enc_key_map = generate_enc_key_map keymap
	let dec_key_map = generate_dec_key_map keymap
	
	putStrLn ("Enc Map: " ++ enc_key_map)
	putStrLn ("Dec Map: " ++ dec_key_map)

	let ciphertext = monoalphabetic_encrypt plaintext enc_key_map
	let dec_plaintext = monoalphabetic_decrypt ciphertext dec_key_map

	putStrLn ("Encrypted Ciphertext: " ++ ciphertext)
	putStrLn ("Decrypted Plaintext: " ++ dec_plaintext)
 
