'''
https://www.hackerrank.com/contests/juniper-2016/challenges/bus-1

Bus and Time

One day, when Meera was standing in a bus stop, she noticed that the bus always comes in every K minutes.
She also got the information that the bus takes exactly T minutes to travel from its first stop to the last stop.
The bus needs another T minutes to get back to the first stop. Here, we are ignoring the time to pick up the passengers and drop the passengers and also assuming that a new bus is always released from its first stop.

Now she wonders, what is the minimum number of buses the company must release every day.

Input Format
The first line contains two integers,  K and T.

Output Format
Single integer representing the answer.

Sample Input
10 20
Sample Output
4
'''

import math

k,t = map(int, raw_input().split())

print int(math.ceil(t*2.0/k))
