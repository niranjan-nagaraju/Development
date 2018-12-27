'''
https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem

Alice is playing an arcade game and wants to climb to the top of the leaderboard and wants to track her ranking. The game uses Dense Ranking, so its leaderboard works like this:

    The player with the highest score is ranked number 1 on the leaderboard.
    Players who have equal scores receive the same ranking number, and the next player(s) receive the immediately following ranking number.

For example, the four players on the leaderboard have high scores of 100, 90, 90 and 80.
Those players will have ranks 1, 2, 2, and 3, respectively. If Alice's scores are 70, 80 and 105, her rankings after each game are 4th, 3rd and 1st. 

Function Description
Complete the climbingLeaderboard function in the editor below. It should return an integer array where each element res[j] represents Alice's rank 
after the jth game.

climbingLeaderboard has the following parameter(s):
    scores: an array of integers that represent leaderboard scores
    alice: an array of integers that represent Alice's scores

Input Format
The first line contains an integer `n, the number of players on the leaderboard.
The next line contains n space-separated integers scores[i], the leaderboard scores in decreasing order.
The next line contains an integer, m, denoting the number games Alice plays.
The last line contains space-separated integers alice[j], the game scores.


Constraints
The existing leaderboard, scores, is in descending order
Alice's scores, alice, are in ascending order.

Output Format

Print m integers. The jth integer should indicate Alice's rank after playing the jth game. 


Sample Input 1:
7
100 100 50 40 40 20 10
4
5 25 50 120


Sample Output 2:
6
4
2
1


Explanation:
Initial ranking: 1 1 2 3 3 4 5
Alice[0]: 5, ranking 6
Alice[1]: 25, ranking 4
Alice[2]: 50, ranking 2
Alice[3]: 120, ranking 1


Sample Input 2:
6
100 90 90 80 75 60
5
50 65 77 90 102



Sample Output 2:
6
5
4
2
1

Explanation:
Initial ranking: 1 2 2 3 4 5
Alice[0]: 50, ranking 6
Alice[1]: 65, ranking 5
Alice[2]: 77, ranking 4
Alice[3]: 90, ranking 2
Alice[4]: 102, ranking 1
'''

#!/bin/python

import math
import os
import random
import re
import sys


# Calculate and return a list of ranking leaderboard for the scores
def calculate_ranking(scores):
	ranks = []
	current_rank = 1

	current_score = scores[0]
	i = 0
	while i < len(scores):
		try:
			# Loop until scores match current score
			# This should match atleast once
			while scores[i] == current_score:
				#print 'same [%d] %d:' %(i, scores[i])
				ranks.append(current_rank)
				i += 1

			# We just hit a different score than the previous one in the sequence
			# Mark this as current score, Increase ranking
			# to be updated by the inner loop until scores[i] match current score
			#print 'Different [%d] %d:' %(i, scores[i])
			current_score = scores[i]
			current_rank += 1
		except IndexError:
			# ran out of scores to match, we are done calculating ranks
			break

	return ranks



# Calculates Alice's rank based on her jth game's score and returns it
# Also return the last index in the leaderboard, that Alice was still found lagging
# The next game when Alice scores more than current game, we'll just start looking from this last index
# as it is wasteful to start all the way from the lowest score in the leaderboard.
# e.g., 
#	scores: [100 90 80], with ranks [1,2,3], Alice scored 85 in game 1, last idx = 1 
#   Next she scores, 89, we don't have to start looking from 80, we can just look at the leaderboard [100, 90], and rank her 3rd, last idx = 1
#   Now in game #3, if she scores, 95, her rank is 2, last idx is 0, 
#   In game #4, she scores 99, we need only look at the leaderboard [100] and rank her second
def rankAlice(scores, ranks, lastIdx, alice_game_score):
	# Rank Alice's score from the current game, in the 'scores' leaderboard
	# consider only [0..lastIdx] portion of the leaderboard
	# since we don't care how Alice's moving up in a leaderboard affects the ranking of other peoples' ranks
	# Alice's scores only ever move upwards, since her scores are sorted in increasing order

	if alice_game_score > scores[0]:
		return (1, 0)

	for i in range(lastIdx, -1, -1):
		# Skip all scores that are lesser than Alice's
		if alice_game_score < scores[i]:
			break

	# At this point, we have an anchor based on which we can rank Alice's score
	# It can either be that scores[i] >= Alice's game score
	# Case 1: scores[i] == Alice's game score
	#   something like  scores: . . . [100, 50], . ., and Alice's game score is 100
	#   if 50 was ranked 5, and 100 was ranked 4, we have to rank Alice's score as 4 (== ranks[i])
	# Case 2: scores[i] > Alice's game score
	#   e.g,  . . . [100, 50], . . ., and Alice's game score is 75
	#   if 50 was ranked 5, and 100 was ranked 4, we have to rank Alice's score as the new 5 (== ranks[i]+1)
	#   NOTE: 50 will now be ranked 6, but we don't have to bother updating it
	if scores[i] == alice_game_score:
		return (ranks[i], i)
	else: # scores[i] > alice_game_score
		return (ranks[i]+1, i)





# Return a list of Alice's rankings in the leaderboard after each game jth score
def climbingLeaderboard(scores, alice):
	leaderboard_before_alice = calculate_ranking(scores)
	alice_rankings = []

	last_idx = len(scores) - 1
	for alice_game_score in alice:
		(rank, last_idx) = rankAlice(scores, leaderboard_before_alice, last_idx, alice_game_score)
		alice_rankings.append(rank)

	return alice_rankings



if __name__ == '__main__':
	# Test ranking calculator / leaderboard maker
	assert( calculate_ranking([100, 100, 90, 90, 80, 70, 60, 60]) == [1,1,2,2,3,4,5,5])
	assert( calculate_ranking([100, 90, 80, 70, 60]) == [1,2,3,4,5])
	assert( calculate_ranking([100, 100, 50, 40, 40, 20, 10]) == [1,1,2,3,3,4,5])
	assert( calculate_ranking([100, 90, 90, 80, 75, 60]) == [1,2,2,3,4,5])

	# Test ranking mechanism based on Alice's scores, and the current leaderboard
	# Input set 1
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 5) == (6, 6)
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 10) == (5, 5)
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 25) == (4, 4)
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 50) == (2, 1)
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 100) == (1, 0)
	assert rankAlice([100, 100, 50, 40, 40, 20, 10], calculate_ranking([100, 100, 50, 40, 40, 20, 10]), 6, 120) == (1, 0)


	# Test ranking mechanism based on Alice's scores, and the current leaderboard
	# Input set 1
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 50) == (6, 5)
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 65) == (5, 4)
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 77) == (4, 3)
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 90) == (2, 0)
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 100) == (1, 0)
	assert rankAlice([100, 90, 90, 80, 75, 60], calculate_ranking([100, 90, 90, 80, 75, 60]), 5, 102) == (1, 0)


	# Test climbingLeaderboard() for sample input 1 and 2
	assert(climbingLeaderboard([100, 100, 50, 40, 40, 20, 10], [5, 25, 50, 120]) == [6,4,2,1])
	assert(climbingLeaderboard([100, 90, 90, 80, 75, 60], [50, 65, 77, 90, 102]) == [6,5,4,2,1])

	'''
	#fptr = open(os.environ['OUTPUT_PATH'], 'w')
	fptr = sys.stdout

	scores_count = int(raw_input())

	scores = map(int, raw_input().rstrip().split())

	alice_count = int(raw_input())

	alice = map(int, raw_input().rstrip().split())

	result = climbingLeaderboard(scores, alice)

	fptr.write('\n'.join(map(str, result)))
	fptr.write('\n')

	fptr.close()
	'''
