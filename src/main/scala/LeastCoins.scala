/**
  * Created by boriska on 10/1/16.
  *
  * Given an integer amount, and a list of possible coin denominations
  * return the shortest list of coins that add up to the amount or None if
  * it is not possible.
  * Assume an infinite supply of coins of the given denomination.
  */
object LeastCoins {

  type Result = Option[List[Int]]

  def shortest(r1: Result, r2: Result): Result = {
    if (r2.isEmpty) r1
    else if (r1.isEmpty) r2
    else if (r1.get.size < r2.get.size) r1 else r2
  }

  /**
    * @return Some(List[Int]) the shortest list of coin denomination to make the amount, or None if
    *         it is not possible to do.
    */
  def findLeastCoins(amount: Int, denominations: List[Int]): Result = {
    if (amount < 0) {
      None // Unable to compose the amount from denominations
    } else if (amount == 0) {
      Some(List()) // Got the exact amount.
    } else if (denominations == Nil) {
      None // Out of coin denominations to try.
    } else {
      // Decrement the amount by the head coin, but still use the head coin denomination.
      val r1 = findLeastCoins(amount - denominations.head, denominations)
      // Decrement the amount, and remove the head coin denomination from consideration.
      val r2 = findLeastCoins(amount - denominations.head, denominations.tail)
      // Don't decrement the amount, remove the head coin denomination from consideration.
      val r3 = findLeastCoins(amount, denominations.tail)

      // Add the head coin to the shortest of r1 and r2, and return
      // the shortest of the above and r3.
      // Note: map is on Optional[List[Int]], not on the List[Int]
      shortest(
        shortest(r1, r2).map(denominations.head :: _),
        r3)
    }
  }

  def main(args: Array[String]): Unit = {
    println(findLeastCoins(26, List(1, 9, 10)))
    println(findLeastCoins(27, List(1, 9, 10)))
    println(findLeastCoins(27, List(2, 10)))
  }
}
