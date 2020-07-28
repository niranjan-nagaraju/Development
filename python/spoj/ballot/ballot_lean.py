import math

city_populations = [0]*500000

# find number of ballots required for a specified number of people/ballot
def find_ballots_allocation(n, num_people_per_ballot):
	ballots = 0
	for i in xrange(n):
		p = city_populations[i]
		ballots += int(math.ceil(p*1.0/num_people_per_ballot))
	return ballots


# Find an optimal number of people per ballot with the specified number of ballots
# and city populations
def search_lowerbound(n, num_ballots, minimum_ppl_per_ballot, maximum_ppl_per_ballot):
	# allocate all available ballots to city with least poulation
	# for least number of ppl/ballot
	l = minimum_ppl_per_ballot / num_ballots
	h = maximum_ppl_per_ballot # most number of ppl/ballot
	while l < h:
		mid = l + (h-l)/2
		ballots_required = find_ballots_allocation(n, mid)
		if ballots_required <= num_ballots:
			# Move towards left to increase ballots / decrease people per ballot
			h = mid
		else:
			# number of ballots required exceeds available
			# move towards right to reduce number of ballots / increase people per ballot
			l = mid + 1

	return l



def run_interactive():
	while True:
		n, b = map(int, raw_input().split())
		if (n,b) == (-1, -1):
			return

		city_populations[0] = int(input())
		maximum_population = minimum_population = city_populations[0]
 
		for i in xrange(1,n):
			city_populations[i] = int(input())
			if minimum_population is None or (city_populations[i] < minimum_population):
				minimum_population = city_populations[i]
			elif city_populations[i] > maximum_population:
				maximum_population = city_populations[i]
		print search_lowerbound(n, b, minimum_population, maximum_population)
		raw_input() # blank line



if __name__ == '__main__':
	run_interactive()

