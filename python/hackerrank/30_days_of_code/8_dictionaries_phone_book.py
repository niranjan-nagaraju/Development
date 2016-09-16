'''
https://www.hackerrank.com/challenges/30-dictionaries-and-maps
'''

n = int(raw_input())

phonebook = {}
for i in range(n):
	(name, number) = raw_input().strip().split()
	phonebook[name] = number

	
while True:
	try:
		name = raw_input().strip()

		# Instead of using 'if phonebook.has_key(name)', let the exception catch it
		# when the key doesn't exist in the dictionary
		print '{0}={1}'.format(name, phonebook[name])
	except EOFError:
		# read until EOF
		break
	except KeyError:
		print 'Not found'

