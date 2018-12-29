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


''' 
Solution Approach:
	Calculate Alice's ranking based on her first game (move left to right on the leaderboard calculating her rank)
	For all other subsequent games , use her previous rank to calculate how she stacks up against people ahead of her in the leaderboard.
	Once we have identified Alice's rank for her first game, we use that to deduce ranks of people above her - doing away the need for a ranking table.

	e.g.
	scores: 100 100 90 80
	Alice[0]: 85, going L-R, we calculate her ranking after Game #0 is 3, ahead of '1' other person (idx = (n-1) == 3)

	Now,
	Game #1:
	Alice[1]: 95, 
	We consider scores[0:idx] == scores[0:3] == [100. 100, 90], we also know last rank was 3 (for a score of 85)
	and so 90 must be rank 2, 95 would be the new rank #2. and Alice's ahead of '2' people now (idx = idx -1 == 2)

	Game #2:
	Alice[2]: 96
	We consider scores[0:idx] = scores[0:2] == [100, 100], we know last rank was 2 (for a score of 95)
	100 is rank 1 (lastrank -1), 96 < 100, so 96 is the new #2. (last idx remains the same since rank didnt improve)
'''

import math
import os
import random
import re
import sys


# Calculate Alice's ranking based on Game #0
# return her rank + index in the scores array after which her score should be added
# lastIdx denotes how many scores[] are ahead of Alice, and only that sub-array scores[0:lastIdx]
#  will be considered while calculating her game for the next game
# e.g.,
#   calculate_initial_ranking([100,100, 90, 80], 75) => (4, 3)
#   calculate_initial_ranking([100,100, 90, 80], 120) => (1, -1)
#   calculate_initial_ranking([100,100, 90, 80], 95) => (2, 1)
def calculate_initial_ranking(scores, alice_game_score):
	current_rank = 1
	current_score = scores[0]
	i = 0
	while i < len(scores):
		try:
			# if current score == alice's score, 
			#    then her rank would the same as the rank for curernt score
			# if current score < alice's score,
			#    then her rank would supplant the old rank for current score 
			# In either case, we just return the max current rank counted so far and return
			if current_score <= alice_game_score:
				break

			# skip repetitions
			while scores[i] == current_score:
				i += 1

			# We just hit a different score than the previous one in the sequence
			# Mark this as current score, Increase ranking
			# to be updated by the inner loop until scores[i] match current score
			current_score = scores[i]
			current_rank += 1
		except IndexError:
			# ran out of scores to match
			# This means alice_aame_score is at the bottom of the table,
			# and is a new low, Add +1 to the lowest ranking right now
			current_rank += 1
			break

	return (current_rank, i-1)



# Search backwards in scores[lastIdx:0]
# return her current rank, lastIdx 
# lastIdx denotes how many scores[] are ahead of Alice, and only that sub-array scores[0:lastIdx]
#  will be considered while calculating her game for the next game
def rankAlice(scores, alice_game_score, last_game_score, lastrank, lastIdx):
	# Alice was already ahead in the leaderboard in the previous game
	# She couldn't have done worse now (as her scores are in ascending order)
	if lastIdx < 0:
		return (1, -1)

	# Alice's game score hasn't changed the last game
	# *OR* it's better than the last game 
	#	but still worse than the score at the bottom of the leaderboard,
	# return the same rank. idx from last time
	if (alice_game_score == last_game_score) or (alice_game_score < scores[lastIdx]): 
		return (lastrank, lastIdx)

	if alice_game_score >= scores[0]:
		return (1, -1)

	current_rank = lastrank
	i = lastIdx
	score = scores[i]
	while i >= 0:
		if score > alice_game_score:
			break

		# skip repetitions
		while score == scores[i]:
			i -= 1
	
		# Found next new high rank, move up the rank
		score = scores[i]
		current_rank -= 1

	return (current_rank, i)


# Return a list of Alice's rankings in the leaderboard after each game jth score
def climbingLeaderboard(scores, alice):
	rank, last_idx = calculate_initial_ranking(scores, alice[0])
	alice_rankings = [rank]

	for i in range(1, len(alice)):
		(rank, last_idx) = rankAlice(scores, alice[i], alice[i-1], rank, last_idx)
		alice_rankings.append(rank)

	return alice_rankings



if __name__ == '__main__':
	assert calculate_initial_ranking([100,100, 90, 80], 75) == (4, 3)
	assert calculate_initial_ranking([100,100, 90, 80], 120) == (1, -1)
	assert calculate_initial_ranking([100,100, 90, 80], 95) == (2, 1)
	assert calculate_initial_ranking([100, 100, 50, 40, 40, 20, 10], 5) == (6, 6)

	assert rankAlice([100,100, 90, 80], 80, 70, 4, 3) == (3,2)
	assert rankAlice([100,100, 90, 80], 80, 80, 3, 2) == (3,2)
	assert rankAlice([100,100, 90, 80], 90, 80, 3, 2) == (2,1)
	assert rankAlice([100,100, 90, 80], 99, 80, 3, 2) == (2,1)
	assert rankAlice([100,100, 90, 80], 100, 99, 2, 1) == (1,-1)
	assert rankAlice([100,100, 90, 80], 120, 100, 1, -1) == (1,-1)

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
