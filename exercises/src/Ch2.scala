object Ch2 {

  // 2.5.1
  val abs: (Int) => (Int) = (n) => if (n<0) -n else n

  // 2.5.2
  def isEven(n: Int): Boolean = n % 2 == 0
  def isOdd(n: Int): Boolean = n %2 != 0

  def not( p: Int => Boolean ): Int => Boolean = ! p(_)

  val otherIsOdd: Int => Boolean = not( isEven )

  // 2.6.1

  type Pred[A] = A => Boolean

  def isDivisibleBy(k: Int): Pred[Int] = {
    (x: Int) => x % k == 0
  }

  val otherIsEven: Pred[Int] = isDivisibleBy(2)

  // 2.6.2

  val isDivisibleBy3And5: Pred[Int] = {
    (n: Int) => isDivisibleBy(3)(n) && isDivisibleBy(5)(n)
  }

  val isDivisibleBy3Or5: Pred[Int] = {
    (n: Int) => isDivisibleBy(3)(n) || isDivisibleBy(5)(n)
  }

  def lift[A](p1: Pred[A], p2: Pred[A], f: (Boolean, Boolean) => Boolean): Pred[A] = {
    (n: A) => f( p1(n), p2(n) )
  }

  val betterIsDivBy3And5: Pred[Int] = lift(isDivisibleBy(3), isDivisibleBy(5), _ && _)

  val betterIsDivBy3Or5: Pred[Int] = lift(isDivisibleBy(3), isDivisibleBy(5), _ || _)

  // 2.6.3

  def bind[A, B, C](a: A, p: A => B, f: (B,A) => C): C = {
    f( p(a), a )
  }

  def applyIf[A](a: A, f: A => A, p: A => Boolean): A = {
    // note to self: I want f(a) if p(a) else a
    sys.error("todo")
  }
}
