package recfun

object Main {
    def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
            for (col <- 0 to row)
                print(pascal(col, row) + " ")
            println()
        }

        println(balance("(just an) example".toList))
        println(balance("())(".toList))

        println(countChange(4, List(1, 2)))
    }

    /**
      * Exercise 1
      */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

    /**
      * Exercise 2
      */
    def balance(chars: List[Char]): Boolean = {
        def balanceSolver(cntLeft: Int, chars: List[Char]): Boolean = {
            if (chars.isEmpty)
                cntLeft == 0
            else if (cntLeft < 0)
                false
            else if (chars.head == '(')
                balanceSolver(cntLeft + 1, chars.tail)
            else if (chars.head == ')')
                balanceSolver(cntLeft - 1, chars.tail)
            else
                balanceSolver(cntLeft, chars.tail)
        }

        balanceSolver(0, chars)
    }

    /**
      * Exercise 3
      */
    def countChange(money: Int, coins: List[Int]): Int = {
        def countChangeSolver(money: Int, coins: List[Int]): Int = {
            if (money < 0 || money > 0 && coins.isEmpty)
                0
            else if (money == coins.head)
                1
            else
                countChangeSolver(money - coins.head, coins) + countChangeSolver(money, coins.tail)
        }

        countChangeSolver(money, coins.sorted)
    }

}