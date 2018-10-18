'''
PLACEHOLDER:
  Use two queues, track current minima, and move from one queue to another when the current minima is dequeued from either queues.
  Enqueue(): Enqueue into current queue whichever it is, update minima.
  Dequeue(): Dequeue from current queue, if dequeued item == minima, then move everything from current queue to the other, make other the current queue.
  minimum(): == minima

      * Indicates Current queue below

      + Enqueue(1)
      *Q1: 1
      min: 1
      Q2:


      + Enqueue(2)
      *Q1: 1 2
      min: 1
      Q2:

      + Enqueue(3)
      *Q1: 1 2 3
      min: 1
      Q2:

      + minimum(): 1

      + Dequeue() -> should return 1
      *Q1: 1 2 3 -> dequeue(Q1) returns 1 == min,
      min: 1
      Q2:
      => move everything else from Q1 to Q2 while updating min, make Q2 current queue.
      Q1: 
      min: 2
      *Q2: 2 3


      + minimum(): 2


      + Enqueue(4)
      Q1: 
      min: 2
      *Q2: 2 3 4

      + minimum(): 2


      + Dequeue() -> should return 2
      Q1: 
      min: 2
      *Q2: 2 3 4 -> dequeue() returns 2 == min => move everything else from Q2 to Q1, make Q1 current (update min while enqueuing one by one into Q1)
      =>
      *Q1: 3 4
      min: 3
      Q2: 

      + minimum(): 3


      + Dequeue() -> should return 3
      *Q1: 3 4 -> dequeue() returns 3 == min => move everything else from Q1 to Q2, make Q2 current (update min while enqueuing one by one into Q2)
      min: 3
      Q2: 
      =>
      Q1: 
      min: 4
      *Q2: 4

      + minimum(): 4

      + Dequeue() -> should return 4
      Q1: 
      min: 4
      *Q2: 4 -> dequeue() returns 4 == min => move everything else from Q2 to Q1, make Q1 current (update min while enqueuing one by one into Q1)
      =>
      *Q1:
      min: 
      Q2: 

'''
