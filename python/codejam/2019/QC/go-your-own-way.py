'''
Go your own way

Use a different path to get from 0,0 to N,N in a NxN maze
dont use same path as the other one
Dont overshoot boundaries


replace first E with an S, and first S after that with an E
'''

if __name__ == '__main__':
	nTestcases = int(input())
	for i in xrange(nTestcases):
		n = int(input())
		path = raw_input().strip()

		new_path = ''
		for s in path:
			if s == 'E':
				new_path += 'S'
			else:
				new_path += 'E'

		print 'Case #{0}: {1}'.format(i+1, new_path)

