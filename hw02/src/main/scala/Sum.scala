// sum(1) = 2
// sum(2) = 2 + 4
// sum(3) = 2 + 4 + 8
// sum(4) = 2 + 4 + 8 + 16 = 2*(1 + (2 + 4 + 8))
object Sum {
  def sum(n : Int):Int = {
     if(n==0) 0
     else 2 * (1 + sum(n-1))
  }
}
