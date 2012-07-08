from all_permutations import permute_all

def main():
	n = int(input())
	a = range(1, n+1)
	permutations = permute_all(a, n)

	permutations.sort()
	for i in permutations:
		print i


if __name__ == "__main__":
	main()
