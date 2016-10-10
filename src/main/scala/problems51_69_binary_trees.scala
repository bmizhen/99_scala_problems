/**
  * Created by boriska on 10/5/16.
  */
object problems51_69_binary_trees {

  abstract class Tree[+A] {
    def nodes: Int

    def height: Int
  }

  case object None extends Tree[Nothing] {
    val nodes = 0
    val height = 0
  }

  case class Node[A](left: Tree[A], right: Tree[A], value: A) extends Tree[A] {
    def this(value: A) = this(None, None, value)

    val height = Math.max(left.height, right.height)
    val nodes: Int = left.nodes + right.nodes + 1
  }


  /**
    * Construct completely balanced binary trees.
    * In a completely balanced binary tree, the following property holds for
    * every node: The number of nodes in its left subtree and the number of
    * nodes in its right subtree are almost equal, which means their difference
    * is not greater than one. Define an object named Tree. Write a function
    * Tree.cBalanced to construct completely balanced binary trees for a given
    * number of nodes. The function should generate all solutions. The function
    * should take as parameters the number of nodes and a single value to put in
    * all of them.
    *
    * scala> Tree.cBalanced(4, "x")
    * res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))),
    * T(x T(x . .) T(x T(x . .) .)), ...
    */
  def cBalanced[A](n: Int, value: A): List[Tree[A]] = {
    if (n == 0) List(None)
    else if (n == 1) List(new Node(value))
    else {
      val left = cBalanced((n - 1) / 2, value)
      val right = cBalanced(n / 2, value)

      for (l <- left; r <- right) yield {
        Node(l, r, value) :: (if (l == r) List() else List(Node(r, l, value)))
      }
    }.flatten
  }

  /**
    * Symmetric binary trees.
    * Let us call a binary tree symmetric if you can draw a vertical line
    * through the root node and then the right subtree is the mirror image
    * of the left subtree. Add an isSymmetric method to the Tree class to
    * check whether a given binary tree is symmetric. Hint: Write an isMirrorOf
    * method first to check whether one tree is the mirror image of another.
    * We are only interested in the structure, not in the contents of the nodes.
    *
    * scala> Node('a', Node('b'), Node('c')).isSymmetric
    * res0: Boolean = true
    */
  def isMirror(l: Tree[_], r: Tree[_]): Boolean = (l, r) match {
    case (None, None) => true
    case (l: Node[_], r: Node[_]) =>
      isMirror(l.left, r.right) && isMirror(l.right, r.left)
    case _ => false
  }

  def isSymmetric(t: Tree[_]): Boolean = t match {
    case None => true
    case n: Node[_] => isMirror(n.left, n.right)
  }

  /**
    * Binary search trees (dictionaries).
    * Write a function to add an element to a binary search tree.
    * scala> End.addValue(2)
    * res0: Node[Int] = T(2 . .)
    *
    * scala> res0.addValue(3)
    * res1: Node[Int] = T(2 . T(3 . .))
    *
    * scala> res1.addValue(0)
    * res2: Node[Int] = T(2 T(0 . .) T(3 . .))
    *
    * Use that function to construct a binary tree from a list of integers.
    *
    * scala> Tree.fromList(List(3, 2, 5, 7, 1))
    * res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
    * Finally, use that function to test your solution to P56.
    *
    * scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
    * res4: Boolean = true
    *
    * scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
    * res5: Boolean = false
    */
  def addValue[U](value: U, tree: Tree[U])
                 (implicit ev:U => Ordered[U]): Tree[U] = tree match {
    case None => new Node(value)
    case x: Node[U] =>
      if (value == x.value) {
        x
      } else if (value < x.value) {
        Node(addValue(value, x.left), x.right, x.value)
      } else {
        Node(x.left, addValue(value, x.right), x.value)
      }
  }

  def fromList[U](l: List[U])
                 (implicit ev:U => Ordered[U]): Tree[U] = {
    val t: Tree[U] = None
    l.foldLeft(t)((x, y: U) => addValue(y, x))
  }

  /**
    * Generate-and-test paradigm.
    * Apply the generate-and-test paradigm to construct all symmetric,
    * completely balanced binary trees with a given number of nodes.
    * scala> Tree.symmetricBalancedTrees(5, "x")
    * res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)),
    * T(x T(x T(x . .) .) T(x . T(x . .))))
    */
  def symmetricBalancedTrees[A](n: Int, value: A) = {
    cBalanced(n, value).filter(isSymmetric(_))
  }

  /**
    * Construct height-balanced binary trees.
    * In a height-balanced binary tree, the following property holds for
    * every node: The height of its left subtree and the height of its right
    * subtree are almost equal, which means their difference is not greater
    * than one.
    * Write a method Tree.hbalTrees to construct height-balanced binary trees
    * for a given height with a supplied value for the nodes. The function
    * should generate all solutions.
    *
    * scala> Tree.hbalTrees(3, "x")
    * res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .))
    * T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
    */
  def hbalTrees[A](h: Int, value: A): List[Tree[A]] = {
    if (h < 1) List(None)
    else if (h == 1) List(new Node(value))
    else {
      for {h1 <- hbalTrees(h - 1, value)
           h2 <- hbalTrees(h - 2, value)} yield {
        List(Node(h1, h2, value), Node(h1, h1, value), Node(h2, h1, value))
      }
    }.flatten
  }
}
