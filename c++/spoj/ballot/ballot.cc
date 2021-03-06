/**
https://www.spoj.com/problems/BALLOT/

BALLOT - Distributing Ballot Boxes

Today, besides SWERC’11, another important event is taking place in Spain which rivals it in importance: General Elections. Every single resident of the country aged 18 or over is asked to vote in order to choose representatives for the Congress of Deputies and the Senate. You do not need to worry that all judges will suddenly run away from their supervising duties, as voting is not compulsory.

The administration has a number of ballot boxes, those used in past elections. Unfortunately, the person in charge of the distribution of boxes among cities was dismissed a few months ago due to financial restraints. As a consequence, the assignment of boxes to cities and the lists of people that must vote in each of them is arguably not the best. Your task is to show how efficiently this task could have been done.

The only rule in the assignment of ballot boxes to cities is that every city must be assigned at least one box. Each person must vote in the box to which he/she has been previously assigned. Your goal is to obtain a distribution which minimizes the maximum number of people assigned to vote in one box.

In the first case of the sample input, two boxes go to the first city and the rest to the second, and exactly 100,000 people are assigned to vote in each of the (huge!) boxes in the most efficient distribution. In the second case, 1, 2, 2 and 1 ballot boxes are assigned to the cities and 1,700 people from the third city will be called to vote in each of the two boxes of their village, making these boxes the most crowded of all in the optimal assignment.

Input
The first line of each test case contains the integers N (1 ≤ N ≤ 500,000), the number of cities, and B (N ≤ B ≤ 2,000,000), the number of ballot boxes. Each of the following N lines contains an integer ai, (1 ≤ ai ≤ 5,000,000), indicating the population of the ith city.
A single blank line will be included after each case. The last line of the input will contain -1 -1 and should not be processed.

Output
For each case, your program should output a single integer, the maximum number of people assigned to one box in the most efficient assignment.

Sample Input
2 7
200000
500000

4 6
120
2680
3400
200

-1 -1

Sample Output
100000
1700
*/


/**
Solution Outline:
	Example
	2 cities, 7 ballots
	city #1: 200000 = 200k
	city #2: 500000 = 500k

	Minimize the Maximum number of people assigned to each ballot.

	If max is the maximum number people assigned to each ballot,
	then,
		lowest max: 1 person per ballot (needs way too many ballots, definitely >= 7)
		highest max: 50k (where each city gets exactly 1 ballot, number of ballots = 2 = number of cities)


	1                  500k (max number of people/ballot)
	  ----------------
	700k                2  (ballots)

	Moving to the left end of the spectrum needs greater number of ballots but also decreases number of people/ballot
	Moving to the right end of the spectrum needs lesser number of ballots but increases numnber of people/ballot.

	The answer lies somewhere in between.

	l = 1, h = 500k
	mid = (1+500k)/2 = 250k

	at 250k people per ballot,
		ballots allocation:
			city #1: 1
			city #2: 2
		Total ballots = 3 <= 7
	Move towards left to increase ballots
	h = mid = 250k
	mid = (1+250k)/2 = 125k

	at 125k people per ballot,
		ballots allocation:
			city #1: 2
			city #2: 4
		Total ballots = 6 <= 7
	Move towards left to increase ballots
	h = mid = 125k
	mid = (1+125k)/2 = 62500

	at 62500 people per ballot,
		ballots allocation:
			city #1: 4
			city #2: 8
		Total ballots = 12 >7
	Move right to decrease ballots
	l = mid + 1 = 62500+1 = 62501
	mid = (62501+125k)/2 == 93750

	at 93750 people per ballot,
		ballots allocation:
			city #1: 3
			city #2: 6
		Total ballots = 9 >7
	Move right to decrease ballots
	l = mid + 1 = 93750+1 = 93751
	mid = (93751+125k)/2 == 109375

	at 109375 people per ballot,
		ballots allocation:
			city #1: 2
			city #2: 5
		Total ballots = 7 <=7
	Move towards left to try and get a better allocation for 7 ballots
	h = mid = 109375
	mid = (93751+109375)/2 = 101563
		
	at 101563 people per ballot,
		ballots allocation:
			city #1: 2
			city #2: 5
		Total ballots = 7 <=7
	Move towards left to try and get a better allocation for 7 ballots
	h = mid = 101563
	mid = (93751+101563)/2 = 97657

	at 97657 people per ballot,
		ballots allocation:
			city #1: 3
			city #2: 6
		Total ballots = 9 >7
	Move right to decrease ballots
	l = mid + 1 = 97657+1 = 97658
	mid = (97658+101563)/2 == 99610
	-- eventually gravitates towards 100k
*/
#include <cassert>
#include <cmath>
#include <iostream>

int city_populations[500000];

// find number of ballots required for a specified number of people/ballot
int find_ballots_allocation(int nCities, int num_people_per_ballot)
{
	int ballots = 0;

	for (int i=0; i<nCities; i++) {
		ballots += ceil(city_populations[i]*1.0/num_people_per_ballot);
	}

	return ballots;
}

// Find an optimal number of people per ballot with the specified number of ballots
// and city populations
int search_lowerbound(int nCities, int num_ballots, int min_population, int max_population)
{
	// allocate all available ballots to city with least poulation
	// for least number of ppl/ballot
	int l = min_population / num_ballots;
	int h = max_population; // most number of ppl/ballot

	while (l < h) {
		int mid = l + (h-l)/2;
		int ballots_required = find_ballots_allocation(nCities, mid);

		if (ballots_required <= num_ballots) {
			// Move towards left to increase ballots / decrease people per ballot
			h = mid;
		} else {
			// number of ballots required exceeds available
			// move towards right to reduce number of ballots / increase people per ballot
			l = mid + 1;
		}
	}
	return l;
}

void run_interactive(void)
{
	int n, b;
	int min_population, max_population;

	while(1) {
		std::cin >> n >> b;
		if (n == -1 && b == -1)
			return;

		std::cin >> city_populations[0];
		min_population = max_population = city_populations[0];

		for (int i=1; i<n; i++) {
			std::cin >> city_populations[i];

			if (city_populations[i] < min_population)
				min_population = city_populations[i];
			else if(city_populations[i] > max_population)
				max_population = city_populations[i];
		}
		std::cout << search_lowerbound(n, b, min_population, max_population) << std::endl;
	}
}


int main(void)
{
	{
		city_populations[0] = 200000;
		city_populations[1] = 500000;
		assert( find_ballots_allocation(2, 100000) == 7);
		assert( find_ballots_allocation(2, 99999) == (3+6));
		assert( search_lowerbound(2, 7, 200000, 500000)  == 100000);
	}
	{
		city_populations[0] = 120;
		city_populations[1] = 2680;
		city_populations[2] = 3400;
		city_populations[3] = 200;
		assert( search_lowerbound(4, 6, 120, 3400)  == 1700);
	}

	run_interactive();

	return 0;
}

