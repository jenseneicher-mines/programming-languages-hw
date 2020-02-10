import org.scalatest.FlatSpec

// unit tests for pattern matching example
class PatternMatchingTest extends FlatSpec {

  it should "return the proper tree-traversal order (In-Order)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree) === List(1,2,3,4,5))
  }

  it should "return the proper tree-traversal order (Pre-Order)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree) === List(4,2,1,3,5))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4))
  }

}
  
  
    
  
