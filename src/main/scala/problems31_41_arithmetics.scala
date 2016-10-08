import scala.annotation.tailrec

/**
  * Created by boriska on 10/2/16.
  */
object problems31_41_arithmetics {
  /**
    * Determine whether a given integer number is prime.
    * scala> 7.isPrime
    * res0: Boolean = true
    */

  /**
    * Prime numbers as a Stream
    */
  def primes = {
    // Eratosthenes Sieve algorithm
    def sieve(s: Stream[Int]): Stream[Int] = {
      s.head #:: sieve(s.tail.filter(_ % s.head != 0))
    }

    2 #:: sieve(Stream.from(3, 2) /* Stream of odd numbers */)
  }

  /**
    * @return true if n is a prime number
    */
  def isPrimeFunc(n: Int): Boolean = {
    if (n < 2) return false
    if (n < 4) return true // 2, 3

    val it = primes.iterator // don't hold on to the head of the Stream.

    // Drop primes smaller than sqrt(n), up to the first factor of n.
    val i = it.dropWhile(p => (p * p < n) && (n % p != 0)).next()

    // Check if found a factor or just ran over sqrt(n).
    n % i != 0
  }

  /**
    * Imperative test for prime numbers. 40x faster than isPrimeFunc
    *
    * @return true if n is a prime number
    */
  def isPrime(n: Int): Boolean = {
    if (n < 2) false
    else if (Set(2, 3, 5, 7).contains(n)) true
    else if (n % 2 == 0 || n % 3 == 0) false
    else {
      // All prime numbers are of the form (6 * i + 1) or (6 * i - 1). Converse is false.
      var i = 6
      do {
        if (n % (i - 1) == 0 || n % (i + 1) == 0) {
          return false
        }
        i += 6
      } while (i * i < n)
      true
    }
  }


  /**
    * (**) Determine the greatest common divisor of two positive integer numbers.
    * Use Euclid's algorithm.
    * scala> gcd(36, 63)
    * res0: Int = 9
    */
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)


  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    * Two numbers are coprime if their greatest common divisor equals 1.
    * scala> areCoprimeTo(35, 64)
    * res0: Boolean = true
    *
    */
  def areCoprime(a: Int, b: Int) = gcd(a, b) == 1

  /**
    * Calculate Euler's totient function phi(m).
    * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m)
    * that are coprime to m.
    * scala> totient(10)
    * res0: Int = 4
    */
  def totient(n: Int): Int = (1 to n).filter(areCoprime(n, _)).size


  /**
    * Determine the prime factors of a given positive integer.
    * Construct a flat list containing the prime factors in ascending order.
    * scala> primeFactors(315)
    * res0: List[Int] = List(3, 3, 5, 7)
    */
  def primeFactors(n: Int): List[Int] = {
    def factors(primes: Stream[Int], n: Int): List[Int] = {
      val ps = primes.dropWhile(p => p < n && n % p != 0)
      if (ps.head > n) Nil
      else ps.head :: factors(ps, n / ps.head)
    }

    factors(primes, n)
  }

  /**
    * Determine the prime factors of a given positive integer (2).
    * Construct a list containing the prime factors and their multiplicity.
    * scala> primeFactorMultiplicity(315)
    * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
    */
  def primeFactorMultiplicity(n: Int): List[(Int, Int)] = {
    primeFactors(n).groupBy(identity).mapValues(_.size).toList.sortBy(_._1)
  }

  /**
    * A list of prime numbers.
    * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
    * scala> listPrimesinRange(7 to 31)
    * res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
    */
  def listPrimesinRange(range: Range): List[Int] = {
    (for (p <- range if isPrime(p)) yield p).toList
  }

  /**
    * P40 (**) Goldbach's conjecture.
    * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
    * E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in
    * the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int
    * can represent). Write a function to find the two prime numbers that sum up to a given even integer.
    * scala> goldbach(28)
    * res0: (Int, Int) = (5,23)
    */
  def goldbach(n: Int): (Int, Int) = {
    if (n < 3 || n % 2 != 0) throw new IllegalArgumentException

    val p = primes.find(p => isPrime(n - p)).get
    (p, n - p)
  }

  /**
    * A list of Goldbach compositions.
    * Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
    * scala> printGoldbachList(9 to 20)
    * 10 = 3 + 7
    * 12 = 5 + 7
    * 14 = 3 + 11
    * 16 = 3 + 13
    * 18 = 5 + 13
    * 20 = 3 + 17
    */
  def printGoldbachList(r: Range): Unit = {
    for (n <- ((r.start + 1) / 2) * 2 to r.end by 2) {
      println(n, " = ", goldbach(n))
    }
  }
}
