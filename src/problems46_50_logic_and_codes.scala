import scala.collection.mutable

/**
  * Created by boriska on 10/3/16.
  */

object problems46_50_logic_and_codes {
  /**
    * Truth tables for logical expressions.
    * Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false
    * according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
    * scala> and(true, true)
    * res0: Boolean = true
    *
    * scala> xor(true. true)
    * res1: Boolean = false
    * A logical expression in two variables can then be written as an function of two variables,
    * e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
    * *
    * Now, write a function called table2 which prints the truth table of a given logical expression in two variables.
    * *
    * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    * A     B     result
    * true  true  true
    * true  false true
    * false true  false
    * false false false
    */

  def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  def and(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean) = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(a: Boolean, b: Boolean) = not(and(a, b))

  def nor(a: Boolean, b: Boolean) = not(or(a, b))

  def xor(a: Boolean, b: Boolean) = or(and(a, not(b)), and(not(a), b))

  def impl(a: Boolean, b: Boolean) = not(and(a, not(b)))

  def equ(a: Boolean, b: Boolean) = not(xor(a, b))


  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    for {a <- List(true, false)
         b <- List(true, false)
    } {
      println(a, b, f(a, b))
    }
  }

  /**
    * Gray code.
    * An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
    * n = 1: C(1) = ("0", "1").
    * n = 2: C(2) = ("00", "01", "11", "10").
    * n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
    * Find out the construction rules and write a function to generate Gray codes.
    * *
    * scala> gray(3)
    * res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
    * See if you can use memoization to make the function more efficient.
    */
  def gray(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val c = gray(n - 1)
      c.map("0" + _) ++ c.reverse.map("1" + _)
    }
  }

  /**
    * Huffman code.
    * First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman
    * codes! We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples.
    * E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)).
    * Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
    *
    * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
    */
  object huffman {
    abstract class Encoder extends Ordered[Encoder] {
      val freq: Int
      def compare(that: Encoder): Int = that.freq - this.freq
    }
    case class Node(zero: Encoder, one: Encoder, freq: Int) extends Encoder
    case class Leaf(v: String, freq: Int) extends Encoder

    def huffman(freqs: List[(String, Int)]):List[(String, String)] = {
      val queue = mutable.PriorityQueue[Encoder]() ++ freqs.map(t => Leaf(t._1, t._2))

      while (queue.size > 1) {
        val n1 = queue.dequeue()
        val n2 = queue.dequeue()
        queue.enqueue(Node(n1, n2, n1.freq + n2.freq))
      }

      def encoding(encoder:Encoder, path: String):List[(String, String)] = encoder match {
        case Leaf(s, _) => List((s, path))
        case Node(zero, one, _) => encoding(zero, path + "0") ++ encoding(one, path + "1")
      }

      encoding(queue.dequeue(), "").sortBy(_._1)
    }
  }

}


