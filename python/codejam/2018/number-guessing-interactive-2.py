import sys

def solve(a, b):
	m = (a + b) / 2
	print m
	sys.stdout.flush()
	s = raw_input()
	if s == "CORRECT":
		return
	elif s == "TOO_SMALL":
		solve(m+1, b)
	else:
		solve(a, m-1)

T = input()
for _ in xrange(T):
	a, b = map(int, raw_input().split())
	_ = input()
	solve(a + 1, b)
