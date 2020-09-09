'''
https://www.interviewbit.com/problems/letter-phone/

Letter Phone

Given a digit string, return all possible letter combinations that the number could represent.

A mapping of digit to letters (just like on the telephone buttons) is given below.
1       2{abc}   3{def}
4{ghi}  5{jkl}   6{mno}
7{pqrs} 8{tuv}   9{wxyz}
*+      0_       #

The digit 0 maps to 0 itself.
The digit 1 maps to 1 itself.

Input: Digit string "23"
Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
Make sure the returned strings are lexicographically sorted.
'''


'''
Solution Outline:
    Let f(c): return all letter combinations for digit c.
    Consider a single digit "3"
        All possible letters for "3" are ["d", "e", "f"]
        => f("3") = ["d", "e", "f"]
    Now if we prepend another digit to "3", say "23"
        All letters '2' maps to are {abc}
        so all letter combinations for "23" are ["a" + f("3"), "b" + f("3"), "c" + f("3")]
            => f("23") = ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]
    If we add another number, say "5", => "523"
        f("5") = ["j", "k", "l"]
        f("523") = ["j" + f("23"), "k" + f("23"), "l" + f("23")]
            = [ 
                "jad", "jae", "jaf", "jbd", "jbe", "jbf", "jcd", "jce", "jcf"]
                "kad", "kae", "kaf", "kbd", "kbe", "kbf", "kcd", "kce", "kcf"]
                "lad", "lae", "laf", "lbd", "lbe", "lbf", "lcd", "lce", "lcf"]
               ]

        => f(ni..n1) = foreach (f(ni) + f(ni-1..n1)
'''
class Solution:
    def find_letter_combinations(self, digits):
        # Helper function to recursively do a DFS of digits at each depth
        def find_letter_combinations_r(depth, prefix=''):
            if depth == len(digits):
                letter_combinations.append(prefix)
                return

            map(lambda x: find_letter_combinations_r(depth+1, prefix+x), mapping[digits[depth]])

        if not digits:
            return []

        mapping = {
                  '0': '0',
                  '1': '1',
                  '2': 'abc',
                  '3': 'def',
                  '4': 'ghi',
                  '5': 'jkl',
                  '6': 'mno',
                  '7': 'pqrs',
                  '8': 'tuv',
                  '9': 'wxyz',
                }

        letter_combinations = []
        find_letter_combinations_r(0)
        return letter_combinations



if __name__ == '__main__':
    s = Solution()
    assert s.find_letter_combinations("1") == ['1']
    assert s.find_letter_combinations("12") == ['1a', '1b', '1c']
    assert s.find_letter_combinations("23") == ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]
    assert s.find_letter_combinations("73") == ['pd', 'pe', 'pf', 'qd', 'qe', 'qf', 'rd', 're', 'rf', 'sd', 'se', 'sf']

