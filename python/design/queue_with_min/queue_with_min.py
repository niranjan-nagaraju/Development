'''
PLACEHOLDER:

A Queue which supports min() in O(1) 
in addition to the exisitng enqueue() and dequeue() in O(1)  [???]

min(): returns the current minimum in the stack

 Uses two concepts
  1. Stack with minimum
     Stack with minimum() is simpler because there is a 'recency bias'
     push (data, current minima) to the stack instead of just data.
     minimum() -> sp

     + push(3)
     Stack: (3,3)

     + push(2)
     Stack: (2,2) (3,3)
     
     + minimum() -> returns 2

     + push(1)
     Stack: (1,1) (2,2) (3,3)

     + minimum() -> returns 1

     + pop() -> removes 1
     Stack: (2,2) (3,3)

     minimum() -> returns 2

  2. Queue implemented using 2 stacks
     Enqueue() -> always push to S1
     Dequeue() -> always pop from S2, unless S2 is empty, in which case, push everything from S1 to S2 and then pop from S2


     + Enqueue(1)
     S1: 1
     S2:

     + Enqueue(2)
     S1: 2 1
     S2:

     + Enqueue(3)
     S1: 3 2 1
     S2: 

     + Enqueue(4)
     S1: 4 3 2 1
     S2:


     + Dequeue() -> should return 1
     S1: 
     S2: 1 2 3 4    -> pop(S2) -> 2 3 4

     + Enqueue(5)
     S1: 5
     S2: 2 3 4

     + Enqueue(6)
     S1: 6 5
     S2: 2 3 4

     + Dequeue()  -> returns 2
     S1: 6 5
     S2: 2 3 4 -> 3 4

     + Dequeue() -> returns 3
     S1: 6 5
     S2: 3 4 -> 4

     + Dequeue() -> returns 4
     S1: 6 5
     S2: 4 -> []

     + Dequeue() -> returns 5
     S1: 6 5 (push all to S2)
     S2:
     =>
     S1:
     S2: 5 6 -> pop(S2) -> 6
    
     + Dequeue() -> returns 6
     S1:
     S2: 6 -> pop(6) -> []
     =>
     S1:
     S2:



   3. Use 1. and 2. to implement Queue with minimum
      Uses two stacks, S1: regular stack, S2: Stack with minimum

      Enqueue(): pushes items to S1, and stores S1 minimum (s1_min) separately.
      Dequeue(): Pops from S2 if not empty, Else pop() all from S1 into S2(which maintains minima) and pop from S2
      minimum(): min(s1_min, S2.minimum())

      + Enqueue(5)
      S1: 5
      s1_min: 5
      S2:

      + Enqueue(4)
      S1: 4 5
      s1_min: 4
      S2:

      + Enqueue(3)
      S1: 3 4 5
      s1_min: 3
      S2:

      + minimum(): 3

      + Dequeue() -> should return 5
      S1: => push everything from S1 to S2, and flush s1_min
      S2:
      =>
      S1: 
      s1_min: 
      S2: (5,3) (4,3) (3,3) -> pop(S2) -> (4,3) (3,3)


      + minimum(): s2.minimum() == 3


      + Enqueue(2)
      S1: 2
      s1_min: 2
      S2: (4,3) (3,3)

      + minimum(): min(s1_min, S2.minimum()) == min(2, 3) == 3


      + Dequeue() -> should return 4
      S1: 2
      s1_min: 2
      S2: (3,3)

      + Dequeue() -> should return 3
      S1: 2
      s1_min: 2
      S2: []

      + minimum(): s1_min == 2

      + Dequeue() -> should return 2
      S1: (push all to S2)
      =>
      S1:
      s1_min:
      S2: 2  => pop() -> []

'''
