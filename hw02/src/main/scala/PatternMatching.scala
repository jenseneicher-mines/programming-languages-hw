object PatternMatching {

  // sealed means all possible extending classes (Note, Rest, ...)
  // are defined in this source file -- this lets the compiler
  // enumerate all possibilities for classes which extend Symbol


  /*
   * abstract data type for binary trees
   */

  sealed trait Tree
  case class Node(left:Tree, d:Int, right:Tree) extends Tree
  case class Leaf(d:Int) extends Tree

  // in-order traversal of binary tree
  // Unit type for functions that don't return anything
  def traverse(t : Tree) : Unit = t match {
    case Node(l, d, r) => {
      traverse(l)
      println(d)
      traverse(r)
    }
    case Leaf(d) => println(d)
  }

  // returns the proper order for the In-Order traversal (as a list)
  def traverse2(t : Tree) : List[Int] = t match {
    case Node(l, d, r) => {
      traverse2(l) ++ List(d) ++ traverse2(r)
    }
    case Leaf(d) => List(d)
  }

  // returns the proper order for the Pre-Order traversal (as a list)
  def traverse2Pre(t : Tree) : List[Int] = {
    // TODO
    List()
  }

  // returns the proper order for the Post-Order traversal (as a list)
  def traverse2Post(t : Tree) : List[Int] = {
    // TODO
    List()
  }

  /*
   this encodes the following tree:
   4
   / \
   2   5
   / \
   1   3
   */

  val myTree = Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5))
  val v = traverse2(myTree) // should produce the value List(1, 2, 3, 4, 5)
}
