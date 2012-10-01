package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def fatorial(num: Int): Int = {
      if (num <= 1)
        return 1
      else
        return num * fatorial(num - 1)
    }

    return fatorial(r) / (fatorial(c) * fatorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var count: Int = 0;
    def nestedBalance(chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        return true
      else if (chars.head == '(')
        count = count + 1
      else if (chars.head == ')')
        count = count - 1
      return count >= 0 && nestedBalance(chars.tail);
    }
    return nestedBalance(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
