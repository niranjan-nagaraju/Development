'''
https://www.interviewbit.com/problems/justified-text/

Justified Text

Given an array of words and a length L, format the text such that each line has exactly L characters and is fully (left and right) justified.
You should pack your words in a greedy approach; that is, pack as many words as you can in each line.

Pad extra spaces ' ' when necessary so that each line has exactly L characters.
Extra spaces between words should be distributed as evenly as possible.
If the number of spaces on a line do not divide evenly between words, the empty slots on the left will be assigned more spaces than the slots on the right.
For the last line of text, it should be left justified and no extra space is inserted between words.

Your program should return a list of strings, where each string represents a single line.

Example:
words: ["This", "is", "an", "example", "of", "text", "justification."]
L: 16.

Return the formatted lines as:

[
   "This    is    an",
   "example  of text",
   "justification.  "
]
Note: Each word is guaranteed not to exceed L in length. 
'''

class Solution:
	def justify_text(self, words, l):
		# Add a single word to current line
		def add_single_word():
			# If there's a single word in current line
			# left-align it and fill the right-side with spaces
			num_spaces = l-len(words[i-1])
			line = words[i-1] + space_char * (num_spaces)
			assert len(line) == l
			lines.append(line)

		
		# Add words with spaces to current line
		def add_words_to_line():
			# We have filled current line with as many words possible
			# check how many spaces to add and distribute them
			total_spaces = l-current_capacity

			spaces_per_word = total_spaces/(num_words_in_current_line-1)

			# If total spaces needed couldn't be evenly distributed
			# between the words in current line,
			# Add the extra spaces from the left, until we reach a line-length of 'l'
			additional_spaces = total_spaces % (num_words_in_current_line-1)

			word_idx = i-num_words_in_current_line
			line = ''
			while word_idx < i-1:
				line += words[word_idx] + space_char*(spaces_per_word+1)
				line += space_char if additional_spaces > 0 else ''
				additional_spaces -= 1
				word_idx+=1
			line += words[i-1]
			assert len(line) == l
			lines.append(line)


		# Left-align last line
		def add_words_to_last_line():
			# last line -- Just add spaces between words, and leave it left-justified.
			word_idx = i-num_words_in_current_line
			line = ''
			while word_idx < i-1:
				line += words[word_idx] + space_char
				word_idx+=1
			line += words[i-1]

			# Fill with spaces at the end to reach line-length, l
			line += space_char * (l-len(line))
			assert len(line) == l
			lines.append(line)



		space_char = ' ' # switch to '-' for visibility while debugging

		# return an empty list if there are no words
		if not words or not words[0]:
			return []

		lines = []
		current_capacity = 0
		num_words_in_current_line = 0
		i = 0
		while i < len(words):
			word = words[i]
			if current_capacity + len(word) <= l:
				# Update current word length + 1 for a space for current line capacity
				current_capacity += len(word)+1
				num_words_in_current_line += 1
			else:
				if num_words_in_current_line == 1:
					add_single_word()
				else:
					# Undo space calculated for last word
					current_capacity -= 1
					add_words_to_line()

				# reinitialize a new line
				current_capacity = len(word)+1
				num_words_in_current_line = 1
			i += 1

		add_words_to_last_line()
		return lines




if __name__ == '__main__':
	s = Solution()
	assert  s.justify_text([""], 10) == []
	assert s.justify_text(["This", "is", "an", "example", "of", "text", "justification."], 16) == [
			   "This    is    an",
			   "example  of text",
			   "justification.  "
			   ]
	assert  s.justify_text(["This", "is", "an", "example", "of", "text"], 8) == [
			'This  is', 'an      ',
			'example ',
			'of text '
			]
	assert s.justify_text(["What", "must", "be", "shall", "be."], 12) == [
			'What must be',
			'shall be.   '
			]

