'''
placeholder for a simple trie implementation

Design:
	Implemented as an n-ary tree.
	The root has 'n' nodes (n being the number of characters in the universe),
	  root['A'] -> child node, indicates there is a word that begins with the character, 'A'
	  the child node's properties, such as eow indicates eow status of 'A'
	  Additional properties such as prefix count, and frequency can be added to the child nodes.


Node(root)
|-------+-------+-------|
|  A    |  B    |  C    |
|-------+-------+-------|Items
| f:  0 | f:  0 | f:  0 |
| pc: 0 | pc: 0 | pc: 0 |
| $:  0 | $:  0 | $:  0 |
|---+----+-------+------| 
    |
    + -------+
	         |
			 V
|-------+-------+-------|                           |-------+-------+-------|
|  A    |  B    |  C    |                           |  A    |  B    |  C    |
|-------+-------+-------|Items                      |-------+-------+-------|
| f:  9 | f:  0 | f:  0 |                           | f:  9 | f:  0 | f:  0 |
| pc: 2 | pc: 1 | pc: 1 +----------+                | pc: 2 | pc: 1 | pc: 1 +
| $:  1 | $:  0 | $:  0 |          |                | $:  1 | $:  0 | $:  0 | 
|---+----+-------+------|          |                |---+----+-------+------| 
    |                              |
    +------+                       |
Node       |                       |                 Node     
|-------+--v----+-------|       |--v-----+-------+------|    
|  A    |  B    |  C    |       |  A    |  B    |  C    |
|-------+-------+-------|       |-------+-------+-------|
| f:  0 | f:  2 | f:  0 |       | f:  1 | f:  0 | f:  0 |
| pc: 0 | pc: 1 | pc: 0 |       | pc: 1 | pc: 0 | pc: 0 |
| $:  0 | $:  1 | $:  0 |       | $:  1 | $:  0 | $:  0 |
|-------+-------+-------|       |-------+-------+-------|
Items                                               Items
  
'''
