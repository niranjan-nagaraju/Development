module MonoalphabeticCipher
	where

import IO
import Char

{- Encryption routine -}
monoalphabetic_encrypt :: String -> String -> String
monoalphabetic_encrypt plaintext enc_key_map = 
	 map (\x -> 
			if isAlpha(x)
				then (enc_key_map !! (ord(toLower(x)) - ord('a')))
				else x
		 ) plaintext


{- Decryption routine -}
monoalphabetic_decrypt :: String -> String -> String
monoalphabetic_decrypt ciphertext dec_key_map = 
	 map (\x -> 
			if isAlpha(x)
				then (dec_key_map !! (ord(toUpper(x)) - ord('A')))
				else x
		 ) ciphertext


{- Create a lookup table for encryption; Pre-store as uppercase -}
generate_enc_key_map :: String -> String
generate_enc_key_map keymap = 
	map toUpper keymap


{- Change a character in "Str" to "c" at the specified position "pos" or /* str[i] = c */ -}
change_char_at_pos :: Char -> Int -> String -> String
change_char_at_pos c pos str = (take (pos) str) ++ [c] ++ (drop (pos+1) str)


{- 
 - Helper to Create a reverse lookup table 
 - Recursively changes the reverse lookup keystring one character at a time.
 -}
generate_dec_key_map' :: Int -> String -> String -> String
generate_dec_key_map' index keymap dec_key_map = 
	if (index >= 0)
		then generate_dec_key_map' (index-1) keymap (change_char_at_pos dec_c rev_index dec_key_map)
		else dec_key_map
	where 
		c = (keymap !! index)
		rev_index = ord(toLower(c)) - ord('a')
		dec_c = chr(ord('a') + index)


{- 
 - Create a reverse lookup table for decryption 
 - Initial dec_key_map is a list of 26 a's which are then replaced R-L with reverse lookup letters for decryption
 -}
generate_dec_key_map :: String -> String
generate_dec_key_map keymap = generate_dec_key_map' 25 keymap (replicate 26 'a')


main = do
	let plaintext = "Hello World"
	let keymap = "ZEBRASCDFGHIJKLMNOPQTUVWXY"
	
	let enc_key_map = generate_enc_key_map keymap
	let dec_key_map = generate_dec_key_map keymap
	
	putStrLn ("Enc Map: " ++ enc_key_map)
	putStrLn ("Dec Map: " ++ dec_key_map)

	let ciphertext = monoalphabetic_encrypt plaintext enc_key_map
	let dec_plaintext = monoalphabetic_decrypt ciphertext dec_key_map

	putStrLn ("Encrypted Ciphertext: " ++ ciphertext)
	putStrLn ("Decrypted Plaintext: " ++ dec_plaintext)
 
