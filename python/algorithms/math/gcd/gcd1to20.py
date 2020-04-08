from gcd import gcd

x = 1
intergcd = 1
while (x <= 20):
	intergcd = gcd(x, intergcd)
	print x,intergcd
	x = x + 1

print intergcd

