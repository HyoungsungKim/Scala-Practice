package recfun

object RecFun extends RecFunInterface {

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
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c*r == 0) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(stackCount:Int, targetChars: List[Char]): Int = {
      if (targetChars.isEmpty) stackCount
      else if (targetChars.head == '(') loop(stackCount+1, targetChars.tail)
      else if (targetChars.head == ')' && stackCount > 0) loop(stackCount-1, targetChars.tail)
      else if (targetChars.head == ')' && stackCount <= 0) -1
      else loop(stackCount, targetChars.tail)
    }
    loop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty) 
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else 0
  }
}
