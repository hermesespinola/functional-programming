package recfun

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
    def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) =>  1
      case (c, r) if (c == r) => 1
      case (c, r) => pascal(c, r-1) + pascal(c-1, r-1)
    }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceCount(count: Int, chars: List[Char]): Int =
        if (chars.isEmpty) count
        else {
          val c =
            if      (chars.head == '(') count + 1
            else if (chars.head == ')') scala.math.abs(count - 1)
            else    count

          balanceCount(c, chars.tail)
        }
      balanceCount(0, chars) == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (0, _) => 1
      case (money, _) if money < 0 => 0
      case (_, coins) if coins.isEmpty => 0
      case (money, coins) =>
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

  }
