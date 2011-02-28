{- Find out whether a list is a palindrome. -}
isPalindrome lst =
	(reverse lst) == lst

main = do
	print $ isPalindrome [1,2,3]
	print $ isPalindrome "madamimadam"
	print $ isPalindrome [1,2,4,8,16,8,4,2,1]
