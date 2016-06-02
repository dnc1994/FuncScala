package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

    @volatile var seqResult = false

    @volatile var parResult = false

    val standardConfig = config(
        Key.exec.minWarmupRuns -> 40,
        Key.exec.maxWarmupRuns -> 80,
        Key.exec.benchRuns -> 120,
        Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    def main(args: Array[String]): Unit = {
        val length = 10000
        val chars = new Array[Char](length)
        val threshold = 10
        val seqtime = standardConfig measure {
            seqResult = ParallelParenthesesBalancing.balance(chars)
        }
        println(s"sequential result = $seqResult")
        println(s"sequential balancing time: $seqtime ms")

        val fjtime = standardConfig measure {
            parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
        }
        println(s"parallel result = $parResult")
        println(s"parallel balancing time: $fjtime ms")
        println(s"speedup: ${seqtime / fjtime}")
    }
}

object ParallelParenthesesBalancing {

    /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
    def balance(chars: Array[Char]): Boolean = {
        def balanceSolver(cntLeft: Int, chars: Array[Char]): Boolean = {
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

    /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
    def parBalance(chars: Array[Char], threshold: Int): Boolean = {

        def traverse(from: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
            // acc._1 -> # of ( not matched; acc.2 -> # of ) not matched
            def helper(chars: Array[Char], acc: (Int, Int)): (Int, Int) = {
                if (chars.isEmpty) acc
                else if (chars.head == '(') helper(chars.tail, (acc._1 + 1, acc._2))
                else if (chars.head == ')') {
                    if (acc._1 > 0) helper(chars.tail, (acc._1 - 1, acc._2))
                    else helper(chars.tail, (acc._1, acc._2 + 1))
                }
                else helper(chars.tail, acc)
            }
            helper(chars.slice(from, until), (arg1, arg2))
        }

        def reduce(from: Int, until: Int): (Int, Int) = {
            if (until - from <= threshold) traverse(from, until, 0, 0)
            else {
                val mid = (from + until) / 2
                val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
                val matched = scala.math.min(l._1, r._2)
                (l._1 + r._1 - matched, l._2 + r._2 - matched)
            }
        }

        reduce(0, chars.length) == (0, 0)
    }

    // For those who want more:
    // Prove that your reduction operator is associative!

}
