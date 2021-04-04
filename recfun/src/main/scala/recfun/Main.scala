package recfun
import common._

object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def bal_rec(lefts: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty)
        if (lefts == 0)
          true
        else
          false
      else
        if (chars.head == '(')
          bal_rec(lefts + 1, chars.tail)
        else
          if (chars.head == ')')
            if (lefts == 0)
              false
            else
              bal_rec(lefts - 1, chars.tail)
          else
            bal_rec(lefts, chars.tail)
    bal_rec(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if (money == 0)
      1
    else
      if (coins.isEmpty)
        0
      else
        if (coins.head > money)
          countChange(money, coins.tail)
        else
          countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
