# HOW-TO

cat input.txt | python3 wordle-guesser.py



# Sample run
$ cat sample.in | python3 wordle-guesser.py
Legend: +: green, x: orange, -: blank
 Adding: IRONS ⬛⬛🟨⬛⬛
 Adding: BEAUT ⬛⬛⬛🟨⬛
 Adding: MOULD ⬛🟩🟩🟩🟩
Number of candidates:  2
candidates:
['could', 'would']

$ python3 wordle-guesser.py
Legend: +: green, x: orange, -: blank
audio ----x
spyre ---x-
worth -+xx-
<ctrl-d>
 Adding: audio ⬛⬛⬛⬛🟨
 Adding: spyre ⬛⬛⬛🟨⬛
 Adding: worth ⬛🟩🟨🟨⬛
Number of candidates:  3
candidates:
['motor', 'robot', 'rotor']
