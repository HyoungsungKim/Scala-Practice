package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c == 0 || c == r then 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def loop(checker:Int, charSlice: List[Char]):Int = 
      if checker < 0 || charSlice.isEmpty then checker
      else if charSlice.head == ')' then loop(checker-1, charSlice.tail)
      else if charSlice.head == '(' then loop(checker+1, charSlice.tail)
      else loop(checker, charSlice.tail)

    loop(0, chars) == 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty || money < 0 then 0
    else if money == 0 then 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
