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
        1
      else
        num * fatorial(num - 1)
    }

    fatorial(r) / (fatorial(c) * fatorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var count: Int = 0;
    def nestedBalance(chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        true
      else {
        if (chars.head == '(')
          count = count + 1
        else if (chars.head == ')')
          count = count - 1
        count >= 0 && nestedBalance(chars.tail);
      }
    }
    nestedBalance(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChanger(money: Int, coins: List[Int], cCoin: Int): Int = {
      if (money == 0)
        1
      else if (money < 0)
        0
      else if (cCoin < 0)
        0
      else {
        countChanger(money, coins, cCoin - 1) + countChanger(money - coins(cCoin), coins, cCoin)
      }
    }
    if (money > 0)
      countChanger(money, coins, coins.length - 1)
    else
      0
  }

}
