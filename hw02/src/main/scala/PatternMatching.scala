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
  def traverse2Pre(t : Tree) : List[Int] = t match {
    case Node(l, d, r) => {
      List(d) ++ traverse2Pre(l) ++ traverse2Pre(r)
    }
    case Leaf(d) => List(d)
  }

  // returns the proper order for the Post-Order traversal (as a list)
  def traverse2Post(t : Tree) : List[Int] = t match {
    case Node(l, d, r) => {
      traverse2Post(l) ++ traverse2Post(r) ++ List(d)
    }
    case Leaf(d) => List(d)
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
  val myTree2 = Node(Leaf(3),4,Leaf(5))
  val myTree3 = Node(Node(Node(Leaf(10),15,Leaf(0)),2,Leaf(3)),4,Leaf(5))
  val myTree4 = Node(Leaf(0), 1, Leaf(0))
  val myTree5 = Node(Leaf(4),10,Node(Node(Leaf(11),15,Leaf(8)),3,Leaf(9)))
  val myTree6 = Node(Node(Node(Node(Leaf(2),1,Leaf(3)),2,Leaf(4)),5,Leaf(6)),7,Leaf(8))

  val v = traverse2(myTree) // should produce the value List(1, 2, 3, 4, 5)
  val v2 = traverse2Pre(myTree) // should produce the new value List()
  val v3 = traverse2Post(myTree) // should produce the new value List()
}
