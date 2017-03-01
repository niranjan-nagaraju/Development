'''
Find x, y, z s.t,

  x y z
+ x y z
+ x y z
  -----
= y y y
  -----
'''

# *UNUSED* 123 => [1,2,3]
def three_digits_of_num(num):
	[a,b,c] = map(lambda x: int(x), str(num))[-3:]
	return [a,b,c]


for x in range(1,10):
	for y in range(1,10):
		for z in range(1,10):
			s = 3*100*x + 3*10*y + 3*z
			# if it's 4-digits, extract only last 3-digits
			s = s % 1000

			if s == y * 111:
				print x, y, z
				break


'''
1 4 8

148 * 3 == 444
'''
