
/* tail recursive factorial */

@scala.annotation.tailrec
def factorial(n: Int, acc: Int = 1): Int = {
  if (n == 0)
    acc
  else
    //tail recursion -> optimize
    factorial(n-1, acc*n)
}

assert(factorial(0) == 1)
assert(factorial(5) == 120)
assert(factorial(10) == 3628800)
