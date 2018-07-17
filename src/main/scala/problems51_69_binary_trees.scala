import scala.annotation.tailrec

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

    override val toString: String = ""
  }

  case class Node[A](left: Tree[A], right: Tree[A], value: A) extends Tree[A] {
    def this(value: A) = this(None, None, value)

    val height = Math.max(left.height, right.height) + 1
    val nodes: Int = left.nodes + right.nodes + 1

    override def toString: String = {
      "" + value + {
        if (left != None || right != None) {
          "(" + left + ", " + right + ")"
        } else ""
      }
    }
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
                 (implicit ev: U => Ordered[U]): Tree[U] = tree match {
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
                 (implicit ev: U => Ordered[U]): Tree[U] = {
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
      val tall = hbalTrees(h - 1, value)
      val short = hbalTrees(h - 2, value)

      val tt = for {n1 <- tall; n2 <- tall} yield Node(n1, n2, value)
      val ts = for {n1 <- tall; n2 <- short} yield Node(n1, n2, value)
      val st = for {n1 <- tall; n2 <- short} yield Node(n2, n1, value)

      tt ::: ts ::: st
    }
  }

  /**
    * Consider a height-balanced binary tree of height H. What is the maximum
    * number of nodes it can contain? Clearly, MaxN = 2H - 1. However, what is
    * the minimum number MinN? This question is more difficult. Try to find a
    * recursive statement and turn it into a function minHbalNodes that takes
    * a height and returns MinN.
    *
    * scala> minHbalNodes(3)
    * res0: Int = 4
    * On the other hand, we might ask: what is the maximum height H a
    * height-balanced binary tree with N nodes can have?
    * Write a maxHbalHeight function.
    *
    * scala> maxHbalHeight(4)
    * res1: Int = 3
    * Now, we can attack the main problem: construct all the height-balanced
    * binary trees with a given number of nodes.
    *
    * scala> Tree.hbalTreesWithNodes(4, "x")
    * res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)),
    * T(x T(x . T(x . .)) T(x . .)), ...
    * Find out how many height-balanced trees exist for N = 15.
    */
  def minHbalNodes(height: Int): Int = height match {
    case 0 => 0
    case 1 => 1
    case x => 1 + minHbalNodes(x - 1) + minHbalNodes(x - 2)
  }

  def maxHbalHeight(n: Int): Int = {
    Stream.from(1).dropWhile(minHbalNodes(_) < n).head
  }

  def minHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case x => 1 + minHbalHeight(n / 2)
  }

  def hbalTreesWithNodes[A](nodes: Int, value: A): List[Tree[A]] = {
    (for {i <- minHbalHeight(nodes) to maxHbalHeight(nodes)} yield {
      hbalTrees(i, value).filter(_.nodes == nodes)
    }).flatten.toList
  }

  /**
    * A leaf is a node with no successors. Write a method leafCount to count them.
    * scala> Node('x', Node('x'), End).leafCount
    * res0: Int = 1
    */
  def leafCount[A](tree: Tree[A]): Int = tree match {
    case None => 0
    case Node(None, None, _) => 1
    case Node(l, r, _) => leafCount(l) + leafCount(r)
  }

  /**
    * Collect the leaves of a binary tree in a list.
    * A leaf is a node with no successors. Write a method leafList to collect
    * them in a list.
    * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
    * res0: List[Char] = List(b, d, e)
    */
  def leafList[A](tree: Tree[A]): List[A] = {
    def leafList[A](tree: Tree[A], acc: List[A]): List[A] = tree match {
      case None => acc
      case Node(None, None, value) => value :: acc
      case Node(l, r, _) =>
        leafList(r, leafList(l, acc))
    }

    leafList(tree, Nil)
  }

  /**
    * Collect the internal nodes of a binary tree in a list.
    * An internal node of a binary tree has either one or two non-empty
    * successors. Write a method internalList to collect them in a list.
    * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
    * res0: List[Char] = List(a, c)
    */
  def internalList[A](tree: Tree[A]): List[A] = {
    def internalList(tree: Tree[A], acc: List[A]): List[A] = tree match {
      case None => acc
      case Node(None, None, _) => acc
      case Node(l, r, v) => internalList(l, internalList(r, v :: acc))
    }
    internalList(tree, Nil)
  }

  /**
    * Collect the nodes at a given level in a list.
    * A node of a binary tree is at level N if the path from the root to the
    * node has length N-1. The root node is at level 1. Write a method atLevel
    * to collect all nodes at a given level in a list.
    * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
    * res0: List[Char] = List(b, c)
    * Using atLevel it is easy to construct a method levelOrder which creates
    * the level-order sequence of the nodes. However, there are more efficient
    * ways to do that.
    */
  def atLevel[A](level: Int, tree: Tree[A]): List[A] = {
    def atLevel(level: Int, tree: Tree[A], acc: List[A]): List[A] = tree match {
      case None => acc
      case Node(l, r, v) =>
        if (level == 1) v :: acc
        else atLevel(level - 1, l, atLevel(level - 1, r, acc))
    }
    atLevel(level, tree, Nil)
  }

  /**
    * Construct a complete binary tree.
    * A complete binary tree with height H is defined as follows:
    * The levels 1,2,3,...,H-1 contain the maximum number of nodes
    * (i.e 2(i-1) at the level i, note that we start counting the levels from
    * 1 at the root). In level H, which may contain less than the maximum
    * possible number of nodes, all the nodes are "left-adjusted". This means
    * that in a levelorder tree traversal all internal nodes come first, the
    * leaves come second, and empty successors (the Ends which are not really
    * nodes!) come last. Particularly, complete binary trees are used as data
    * structures (or addressing schemes) for heaps.
    *
    * We can assign an address number to each node in a complete binary tree by
    * enumerating the nodes in levelorder, starting at the root with number 1.
    * In doing so, we realize that for every node X with address A the following
    * property holds: The address of X's left and right successors are 2*A and
    * 2*A+1, respectively, supposed the successors do exist. This fact can be
    * used to elegantly construct a complete binary tree structure. Write a
    * method completeBinaryTree that takes as parameters the number of nodes and
    * the value to put in each node.
    *
    * scala> Tree.completeBinaryTree(6, "x")
    * res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
    */
  def completeBinaryTree[A](nodes: Int, value: A): Tree[A] = {
    def completeBinaryTree(address: Int): Tree[A] = {
      if (address > nodes) None
      else {
        Node(completeBinaryTree(address * 2),
          completeBinaryTree(address * 2 + 1),
          value)
      }
    }

    completeBinaryTree(1)
  }

  /**
    * A string representation of binary trees.
    * Somebody represents binary trees as strings of the following type
    * (see example opposite): a(b(d,e),c(,f(g,)))
    * Write a method which generates this string representation, if the tree is
    * given as usual (in Nodes and Ends). Use that method for the Tree class's
    * and subclass's toString methods. Then write a method (on the Tree object)
    * which does this inverse; i.e. given the string representation, construct
    * the tree in the usual form.
    *
    * For simplicity, suppose the information in the nodes is a single letter
    * and there are no spaces in the string.
    *
    * scala> Node('a', Node('b', Node('d'), Node('e')), Node('
    * c', End, Node('f', Node('g'), End))).toString
    * res0: String = a(b(d,e),c(,f(g,)))
    *
    * scala> Tree.fromString("a(b(d,e),c(,f(g,)))")
    * res1: Node[Char] = a(b(d,e),c(,f(g,)))
    */
  def fromString(s: String): Tree[Char] = {
    def fromList(tokens: List[Char]): (List[Char], Tree[Char]) = tokens match {
      case Nil => (Nil, None)
      case ',' :: _ => (tokens, None) // _(,_)
      case ')' :: _ => (tokens, None) // _(_,)
      case head :: '(' :: tail => {  // a(_, _)
        val (coma, left) = fromList(tail)
        val (closeParen, right) = fromList(coma.tail)
        (closeParen.tail, Node(left, right, head))
      }
      case head :: tail => (tail, new Node(head)) // "a", _(a,_) and _(_,a)
    }

    fromList(s.toList)._2
  }
}
