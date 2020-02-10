import org.scalatest.FlatSpec

// unit tests for pattern matching example
class SumTest extends FlatSpec {
  "Sum" should "return the correct answer for small numbers Sum(8) = 256" in {
    assert(Sum.sum(8) === 256)
  }
  "Sum" should "return the correct answer for small numbers Sum(0) = 1" in {
    assert(Sum.sum(0) === 1)
  }
  "Sum" should "return the correct answer for small numbers Sum(4) = 16" in {
    assert(Sum.sum(4) === 16)
  }
  "Sum" should "return the correct answer for small nmbers Sum(10) = 1024" in {
    assert(Sum.sum(10) === 1024)
  }
  "Sum" should "return the correct answer for small numbers Sum(15) = 32768" in {
    assert(Sum.sum(15) === 32768)
  }
}
