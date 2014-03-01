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
    if (r == 0 || r == 1 || c== 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */ //())(
  def balance(chars: List[Char]): Boolean = {
    def findClosed(count: Int, chars: List[Char]): Boolean = {
      if (count < 0 || count > 0 && chars.isEmpty)
        false
      else if (count == 0 && chars.isEmpty)
        true
      else if (chars.head == '(')
        findClosed(count + 1, chars.tail)
      else if (chars.head == ')')
        findClosed(count - 1, chars.tail)
      else
        findClosed(count, chars.tail)
    }

    findClosed(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && coins.isEmpty)
      0
    else if (money == 0)
      1
    else if ( (money - coins.head) < 0)
      countChange(money, coins.tail)
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
