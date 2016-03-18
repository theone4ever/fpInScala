
/**
  * fib with tail recurision
  * @param n
  * @return
  */
def fib(n: Int) = {
  @annotation.tailrec
  def loop(n: Int, curr: Int, prev: Int): Int = {
    if(n == 0) 0
    else if( n== 1) curr
    else loop(n-1, curr+prev, curr)
  }
  loop(n, 1, 0)
}
fib(2)
fib(3)
fib(5)
fib(8)


def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = {
    if(n == as.length-1) true
    else if(!ordered(as(n-1), as(n))) false
    else loop(n+1)
  }

  loop(1)
}
isSorted(Array(1,2,3,4), (a: Int, b: Int)=> a< b)
isSorted(Array(1,2,0,4), (a: Int, b: Int)=> a< b)


def findFirst[T](ss: Seq[T],  eq: T=> Boolean): Int = {
  @annotation.tailrec
  def loop(index: Int): Int = {
    if(index==ss.length) -1
    else if(eq(ss(index))) index
    else loop(index+1)
  }
  loop(0)
}

findFirst(List("abc","def","a"),  (a: String) => a =="a")









