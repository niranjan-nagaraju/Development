#!/usr/bin/python

'''
A = P (1 + r/n) ^ nt

A = the future value of the investment/loan, including interest
P = the principal investment amount (the initial deposit or loan amount)
r = the annual interest rate (decimal)
n = the number of times that interest is compounded per year
t = the number of years the money is invested or borrowed for 

Sample Usage:
============	
	$ python compound_interest.py -P 2,50,000 -t 7 -r 10  
	4,87,179.275

	$ python compound_interest.py -P 2,50,000 -t 7 -r 10  -v
	Returns after 1 years: 2,75,000.0, Returns: 25,000.0
	Returns after 2 years: 3,02,500.0, Returns: 52,500.0
	Returns after 3 years: 3,32,750.0, Returns: 82,750.0
	Returns after 4 years: 3,66,025.0, Returns: 1,16,025.0
	Returns after 5 years: 4,02,627.5, Returns: 1,52,627.5
	Returns after 6 years: 4,42,890.25, Returns: 1,92,890.25
	Total Value after 7.0 years: 4,87,179.275, Returns: 2,37,179.275

'''

import sys, getopt


# Strip commas, return an integer from a string
def parseNumber (number):
	return float(number.replace(',', ''))

def getInputs (argv):
	try:
		opts, args = getopt.getopt(argv, "P:r:n:t:v", ["principal=", "rate=", "frequency=", "time=", "verbose"])
	except getopt.GetOptError as err:
		print str(err)
		sys.exit(2)

	global P, r, n, t, verbose

	for o, a in opts:
		if o == "-P":
			P = parseNumber(a)
		elif o == "-r":
			r = parseNumber(a) / 100.0
		elif o == "-n":
			n = parseNumber(a)
		elif o == "-t":
			t = parseNumber(a)
		elif o == "-v":
			verbose = True
		else:
			assert False, "Unknown option"


def calculate_compound_interest(P, r, n, t):
	A = P * (( 1 + r/n) ** (n*t))
	return A, A-P

# Add commas according to Hindu-Arabic system
def formatIndianNumeral(n):
	ns = str(n)
	di= ns.find('.')
	numeral,decimals = ns[:di], ns[di:] # or could have just used ns.split('.')
	after_1000s = (numeral[:-3] + "," + numeral[-3:]) 
	after_lacs = after_1000s[:-6] + "," + after_1000s[-6:]
	after_crores = after_lacs[:-9] + "," + after_lacs[-9:]

	# At this point, there might be a lot of commas at the start
	# our values might not even have run into crores -- but the commas are added regardless
	# trim them -- reverse + trim off the end + reverse back
	formatted_value = after_crores[::-1].strip(',')[::-1] + decimals
	return formatted_value

	

P = 10000.0
r = 10/100.0
n = 1.0
t = 1.0
verbose = False

def main():
	getInputs(sys.argv[1:])
	A, returns = calculate_compound_interest(P, r, n, t)
	A = formatIndianNumeral(A)
	returns = formatIndianNumeral(returns)

	if (verbose):
		for i in range(1, int(t)):
			Ai, retsi = calculate_compound_interest(P, r, n, i)
			print "Returns after {0} years: {1}, Returns: {2}".format(i, formatIndianNumeral(Ai), 
					formatIndianNumeral(retsi))
		print "Total Value after {0} years: {1}, Returns: {2}".format(t, A, returns)
		return

	print A


if __name__ == "__main__":
	main()
