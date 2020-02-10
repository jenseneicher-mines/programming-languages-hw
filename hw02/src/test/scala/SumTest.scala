import org.scalatest.FlatSpec

// unit tests for pattern matching example
class SumTest extends FlatSpec {
  "Sum" should "return the correct answer for small numbers Sum(8) = 510" in {
    assert(Sum.sum(8) === 510)
  }
  "Sum" should "return the correct answer for small numbers Sum(0) = 0" in {
    assert(Sum.sum(0) === 0)
  }
  "Sum" should "return the correct answer for small numbers Sum(4) = 30" in {
    assert(Sum.sum(4) === 30)
  }
  "Sum" should "return the correct answer for small nmbers Sum(10) = 2046" in {
    assert(Sum.sum(10) === 2046)
  }
  "Sum" should "return the correct answer for small numbers Sum(15) = 65534" in {
    assert(Sum.sum(15) === 65534)
  }
}
