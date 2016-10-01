import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by boriska on 9/28/16.
  */
object problems1_28_lists {

  /**
    * Find the last element of a list.
    * Example:
    * scala> last(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 8
    */
  @tailrec
  def last[T](l: List[T]): T = l match {
    case Nil => throw new NoSuchElementException()
    case h :: Nil => h
    case _ :: tail => last(tail)
  }

  /**
    * Find the last but one element of a list.
    * Example:
    * scala> penultimate(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 5
    */
  @tailrec
  def penulltimate[T](l: List[T]): T = l match {
    case Nil => throw new NoSuchElementException()
    case p :: _ :: Nil => p
    case _ :: tail => penulltimate(tail)
  }

  /**
    * Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * Example:
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */
  @tailrec
  def kth[T](k: Int, l: List[T]): T = (k, l) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, h :: _) => h
    case (_, _ :: tail) => kth(k - 1, tail)
  }


  /**
    * Find the number of elements of a list.
    * Example:
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */
  def count(l: List[Any]): Int = {
    @tailrec
    def countRec(c: Int, l: List[Any]): Int = l match {
      case Nil => c
      case _ :: tail => countRec(c + 1, tail)
    }
    countRec(0, l)
  }

  /**
    * P05 (*) Reverse a list.
    * Example:
    * scala> reverse(List(1, 1, 2, 3, 5, 8))
    * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    */
  def reverse[T](l: List[T]): List[T] = {
    @tailrec
    def reverseRec(result: List[T], source: List[T]): List[T] = source match {
      case Nil => result
      case h :: tail => reverseRec(h :: result, tail)
    }
    reverseRec(Nil, l)
  }

  /**
    * Find out whether a list is a palindrome.
    * Example:
    * scala> isPalindrome(List(1, 2, 3, 2, 1))
    * res0: Boolean = true
    */
  def isPalindrome[A](l: List[A]): Boolean = l == reverse(l)

  /**
    * Flatten a nested list structure.
    * Example:
    * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    */
  def flatten[_](l: List[_]): List[_] = l match {
    case Nil => Nil
    case (h: List[_]) :: tail => flatten(h) ++ flatten(tail)
    case h :: tail => h :: flatten(tail)
  }

  def flattenFunc[_](l: List[_]): List[_] = l flatMap {
    case xs: List[_] => flattenFunc(xs)
    case e => List(e)
  }

  /**
    * Eliminate consecutive duplicates of list elements.
    * If a list contains repeated elements they should be replaced with a single copy of the element.
    * The order of the elements should not be changed.
    * Example:
    * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    */
  def compress[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail =>
      def dedup[A](last: A, source: List[A]): List[A] = source match {
        case Nil => Nil
        case h :: tail if (source.head == last) => dedup(last, tail)
        case h :: tail => h :: dedup(h, tail)
      }
      h :: dedup(h, tail)
  }

  def compressFunc[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail => h :: compressFunc(tail dropWhile (_ == h))
  }


  /**
    * Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example:
    * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
    * List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    */
  def pack[A](l: List[A]): List[List[A]] = l match {
    case Nil => Nil
    case h :: tail => l.takeWhile(_ == h) :: pack(l.dropWhile(_ == h))
  }

  /**
    * (*) Run-length encoding of a list.
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
    * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    * Example:
    * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encode[A](l: List[A]): List[(Int, A)] = pack(l) map (x => (x.size, x.head))

  /**
    * (*) Modified run-length encoding.
    * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply
    * copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    * Example:
    * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    */
  def encodeModified(l: List[Any]): List[Any] = {
    def el(e: List[Any]): Any = {
      if (e.size > 1) (e.size, e.head)
      else e.head
    }
    pack(l).map(el)
  }

  /**
    * Decode a run-length encoded list.
    * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    * Example:
    * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    */
  def decode[A](l: List[(Int, A)]): List[A] = {
    l flatMap (x => List.fill(x._1)(x._2))
  }

  /**
    * Run-length encoding of a list (direct solution).
    * Implement the so-called run-length encoding data compression method directly.
    * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    * Example:
    * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encodeDirect[A](l: List[A]): List[(Int, A)] = l match {
    case Nil => Nil
    case h :: _ =>
      val (hs, rest) = l span (_ == h)
      (hs.size, hs.head) :: encodeDirect(rest)
  }

  /**
    * Duplicate the elements of a list.
    * Example:
    * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    */
  def duplicate[A](l: List[A]): List[A] = l.flatMap(x => List(x, x))

  /**
    * Duplicate the elements of a list a given number of times.
    * Example:
    * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    */
  def duplicateN[A](n: Int, l: List[A]): List[A] = l.flatMap(x => List.fill(n)(x))

  /**
    * Drop every Nth element from a list.
    * Example:
    * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    */
  def drop[A](n: Int, l: List[A]): List[A] = {
    def dropRec[A](c: Int, l: List[A]): List[A] = (c, l) match {
      case (_, Nil) => Nil
      case (1, h :: tail) => dropRec(n, tail)
      case (_, h :: tail) => h :: dropRec(c - 1, tail)
    }
    dropRec(n, l)
  }

  /**
    * Split a list into two parts.
    * The length of the first part is given. Use a Tuple for your result.
    * Example:
    *
    * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    */
  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitRec(n: Int, l1: List[A], l2: List[A]): (List[A], List[A]) = (n, l2) match {
      case (0, _) => (l1.reverse, l2)
      case (_, Nil) => (l1, l2)
      case (_, h :: tail) => splitRec(n - 1, h :: l1, tail)
    }

    splitRec(n, Nil, l)
  }

  /**
    * Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the
    * Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    * Example:
    *
    * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g)
    */
  @tailrec
  def slice[A](i: Int, k: Int, l: List[A]): List[A] = {
    if (l == Nil) Nil
    else if (i == 0) l.take(k)
    else slice(i - 1, k - 1, l.tail)
  }

  /**
    * Rotate a list N places to the left.
    * Examples:
    * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    *
    * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    */
  def rotate[A](n: Int, l: List[A]): List[A] = {
    val (a, b) = if (n < 0) {
      l.splitAt(l.size + n)
    } else {
      l.splitAt(n)
    }

    b ++ a
  }

  /**
    * Remove the Kth element from a list.
    * Return the list and the removed element in a Tuple. Elements are numbered from 0.
    * Example:
    * *
    * scala> removeAt(1, List('a, 'b, 'c, 'd))
    * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
    */
  def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
    val (a, b) = l.splitAt(n)
    (a ++ b.tail, b.head)
  }

  /**
    * Insert an element at a given position into a list.
    * Example:
    * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
    */
  def insertAt[A](k: Int, l: List[A], e: A): List[A] = {
    val (a, b) = l.splitAt(k)
    a ++ (e :: b)
  }

  /**
    * Create a list containing all integers within a given range.
    * Example:
    * scala> range(4, 9)
    * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
    */
  def range(i: Int, j: Int): List[Int] = {
    if (i > j) List()
    else i :: range(i + 1, j)
  }

  /**
    * Extract a given number of randomly selected elements from a list.
    * Example:
    * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    * res0: List[Symbol] = List('e, 'd, 'a)
    */
  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    if (n == 0) List()
    else {
      val (l2, e) = removeAt(Random.nextInt(l.size), l)
      e :: randomSelect(n - 1, l2)
    }
  }

  /**
    * Lotto: Draw N different random numbers from the set 1..M.
    * Example:
    * scala> lotto(6, 49)
    * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
    */
  def lotto(n: Int, max: Int): List[Int] = {
    randomSelect(n, range(1, max))
  }

  /**
    * Generate a random permutation of the elements of a list.
    * Hint: Use the solution of problem P23.
    * Example:
    *
    * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
    */
  def randomPermute[A](l: List[A]): List[A] = {
    randomSelect(l.size, l)
  }

  /**
    * Generate the combinations of K distinct objects chosen from the N elements of a list.
    * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there
    * are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians,
    * this result may be great. But we want to really generate all the possibilities.
    * Example:
    * *
    * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
    */
  def combinations[A](k: Int, l: List[A]): List[List[A]] = {
    if (k == 0) {
      List(List())
    }
    else if (l == Nil) List()
    else {
      combinations(k - 1, l.tail).map(x => l.head :: x) ++ combinations(k, l.tail)
    }
  }

  /**
    * Group the elements of a set into disjoint subsets.
    * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
    * Write a function that generates all the possibilities.
    * Example:
    *
    * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi),
    * List(Flip, Gary, Hugo, Ida)), ...
    * b) Generalize the above predicate in a way that we can specify a list of group sizes
    * and the predicate will return a list of groups.
    *
    * Example:
    *
    * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
    * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as
    * ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...)
    * and ((Carla, David), (Aldo, Beat), ...).
    *
    * You may find more about this combinatorial problem in a good book on discrete mathematics under
    * the term "multinomial coefficients".
    */
  def group[A](groupSizes: List[Int], l: List[A]): List[List[List[A]]] = groupSizes match {
    case Nil => List(List())
    case headGroupSize :: otherGroupSizes =>
      for {headGroup <- combinations(headGroupSize, l)
           restGroups <- group(otherGroupSizes, l diff headGroup)
      } yield {
        headGroup :: restGroups
      }
  }

  /**
    * Sorting a list of lists according to length of sublists.
    * a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of
    * the list according to their length. E.g. short lists first, longer lists later, or vice versa.
    * Example:
    *
    * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l),
    * List('m, 'n), List('o)))
    * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c),
    * List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    */
  def lsort[A](l: List[List[A]]) = {
    l.sortBy(_.size)
  }

  /**
    * b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to
    * sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly,
    * lists with rare lengths are placed, others with a more frequent length come later.
    *
    * Example:
    *
    * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l),
    * List('m, 'n), List('o)))
    * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h),
    * List('d, 'e), List('d, 'e), List('m, 'n))
    * Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear
    * just once. The third and fourth lists have length 3 and there are two list of this length.
    * Finally, the last three lists have length 2. This is the most frequent length.
    */
  def lsortFreq[A](l:List[List[A]]) = {
    val freqMap = l map (_.size) groupBy(identity) mapValues(_.size)
    l.sortBy(x=>freqMap(x.size))
  }
}
