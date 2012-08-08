def intToStr (x, strList):
	if (x == 0):
		return

	intToStr(x/10, strList)
	strList += [chr((x%10) + ord('0'))]

string = []
intToStr(1234, string)
print "".join(string)
