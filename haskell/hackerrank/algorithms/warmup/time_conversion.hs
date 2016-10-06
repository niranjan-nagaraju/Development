{-
 - https://www.hackerrank.com/challenges/time-conversion
 -
 - Sample Input
 - 07:05:45PM
 -
 - Sample Output
 - 19:05:45
 -}

-- Get 'hh', 'AM/PM' and return 24-hr HH
convert_time' :: [Char] -> [Char] -> [Char] 
convert_time' hours pm_or_am
	| pm_or_am == "AM" = if (hours == "12") then "00" else hours
	| pm_or_am == "PM" = if (hours /= "12") then show $ ((read hours)::Int) + 12 else hours
	
convert_time :: [Char] -> [Char] 
convert_time timeStr =
	let 
		(hours,rest) = splitAt 2 timeStr
		(mm_ss,pm_or_am) = splitAt 6 rest
	in (convert_time' hours pm_or_am) ++ mm_ss


main = do
	str <- getLine
	putStrLn $ (convert_time str)


{-
 - Testcases
 - *Main> convert_time "03:45:12PM"
 - "15:45:12"
 - *Main> convert_time "03:45:12AM"
 - "03:45:12"
 - *Main> convert_time "12:45:12AM"
 - "00:45:12"
 - *Main> convert_time "01:45:12AM"
 - "01:45:12"
 - *Main> convert_time "01:45:12PM"
 - "13:45:12"
 -}
