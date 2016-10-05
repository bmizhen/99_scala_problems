import problems46_50_logic_and_codes._

import scala.collection.mutable

table2((a, b) => and(xor(a, b), impl(a, b)))

gray(3)


abstract class Huffman extends Ordered[Huffman] {
  val freq: Int

  def compare(that: Huffman): Int = that.freq - this.freq
}

case class EncoderNode(zero: Huffman, one: Huffman, freq: Int) extends Huffman

case class EncoderLeaf(v: String, freq: Int) extends Huffman

/**
  * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
  * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
  */
def huffmanEncoder(freqs: List[(String, Int)]):List[(String, String)] = {
  if (freqs.size < 1) throw new IllegalArgumentException("Need at least 2 symbols for encoding")

  val queue = mutable.PriorityQueue[Huffman]() ++ freqs.map(t => EncoderLeaf(t._1, t._2))

  while (queue.size > 1) {
    val n1 = queue.dequeue()
    val n2 = queue.dequeue()
    queue.enqueue(EncoderNode(n1, n2, n1.freq + n2.freq))
  }

  def encoding(encoder:Huffman, path: String):List[(String, String)] = encoder match {
    case EncoderLeaf(s, _) => List((s, path))
    case EncoderNode(zero, one, _) => encoding(zero, path + "0") ++ encoding(one, path + "1")
  }

  encoding(queue.dequeue(), "").sortBy(_._1)
}

huffmanEncoder(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))

23
