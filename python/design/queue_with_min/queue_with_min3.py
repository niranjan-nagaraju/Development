'''
PLACEHOLDER:

  Use two queues, 1 regular queue F1 and another deque F2 (which will be kept sorted but will discard all elements greater than a much smaller element that comes later in time)
  WHY?
    e.g. 3 2 1, 1 is the last minima noticed, and will be the last one to be dequeued (so 3 and 2 will never be relevant as minima as soon as 1 is brought in.

  Enqueue(x):
    add to F1,
    update minima = x if x < minima
    In F2, remove everything from the right if > x, and add x

  Dequeue():
      item = F1.dequeue()
    //Item is the same as F2 minima, remove it from F2 as well
    if item == F2.head():
      F2.dequeue()

      // minima has been removed, so update  minima to current F2 head.
      // this is likely the same as item == F2.head()
      minima = F2.head()


   Minimum():
     == minima


  + Enqueue(4)
    F1: 4
    min: 4
    F2: 4 

  + Enqueue(1)
    F1: 4 1
    min: 1
    F2: (remove all > 1) and add 1
    =>
    F2: 1

  + Enqueue(5)
    F1: 4 1 5
    min: 1
    F2: (remove all > 5) and add 5
    =>
    F2: 1 5


  + Enqueue(3)
    F1: 4 1 5 3
    min: 1
    F2: (remove all > 3), add 3
    =>
    F2: 1 3

  + Minimum(): 1

 
  + Enqueue(2)
    F1: 4 1 5 3 2
    min: 1
    F2: (remove all > 2), add 2
    =>
    F2: 1 2


  + Dequeue(): returns 4
    F1.dequeue() == 4
    =>
    F1: 1 5 3 2
    min: 1
    F2: 1 2

  + Dequeue(): returns 1
    F1.dequeue() == 1
    =>
    F1: 5 3 2
    F2: 1 2
    -> item == F2.head() == 1, remove 1 from F2
    F2: 2
    min: 2
    F1: 5 3 2

  + Dequeue(): returns 5
    data: F1.dequeue() == 5
    =>
    F1: 3 2
    min: 2
    F2: 2

  + Dequeue(): returns 3
    data: F1.dequeue() == 3
    =>
    F1: 2
    min: 2
    F2: 2

  + Enqueue(6):
    F1: 2 6
    min: 2
    F2: (remove all > 6) and add 6
    =>
    F2: 2 6

  + Dequeue():
    F1.dequeue() == 2
    item == F2.head() == min
    F2.dequeue()
    =>
    F1: 6
    min: 6
    F2: 6

'''
