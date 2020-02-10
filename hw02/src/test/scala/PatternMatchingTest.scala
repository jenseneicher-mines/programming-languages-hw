import org.scalatest.FlatSpec

// unit tests for pattern matching example
class PatternMatchingTest extends FlatSpec {

  // IN ORDER TRAVERSAL TESTS

  it should "return the proper tree-traversal order (In-Order)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree) === List(1,2,3,4,5))
  }

  
  it should "return the proper tree-traversal order (In-Order #2)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree2) === List(3,4,5))
  }


  it should "return the proper tree-traversal order (In-Order #3)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree3) === List(10,15,0,2,3,4,5))
  }


  it should "return the proper tree-traversal order (In-Order #4)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree4) === List(0,1,0))
  }


  it should "return the proper tree-traversal order (In-Order #5)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree5) === List(4,10,11,15,8,3,9))
  }

  
  it should "return the proper tree-traversal order (In-Order #6)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree6) === List(2,1,3,2,4,5,6,7,8))
  }

  // PRE ORDER TRAVERSAL TESTS

  it should "return the proper tree-traversal order (Pre-Order)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree) === List(4,2,1,3,5))
  }

  it should "return the proper tree-traversal order (Pre-Order #2)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree2) === List(4,3,5))
  }

  it should "return the proper tree-traversal order (Pre-Order #3)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree3) === List(4,2,15,10,0,3,5))
  }

  it should "return the proper tree-traversal order (Pre-Order #4)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree4) === List(1,0,0))
  }

  it should "return the proper tree-traversal order (Pre-Order #5)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree5) === List(10,4,3,15,11,8,9))
  }

  it should "return the proper tree-traversal order (Pre-Order #6)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree6) === List(7,5,2,1,2,3,4,6,8))
  }

  // POST ORDER TRAVERSAL TESTS

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order #2)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree2) === List(3,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order #3)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree3) === List(10,0,15,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order #4)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree4) === List(0,0,1))
  }

  it should "return the proper tree-traversal order (Post-Order #5)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree5) === List(4,11,8,15,9,3,10))
  }

  it should "return the proper tree-traversal order (Post-Order #6)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree6) === List(2,3,1,4,2,6,5,8,7))
  }

}
  
  
    
  
