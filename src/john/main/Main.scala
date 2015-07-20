package john.main
import john.queens.NQueens
import john.data.Divide
import john.data.None
import john.divideconquer.DivideAndConquer
import john.solver.objects.Value
import john.solver.objects.NoAssignment
import john.solver.Solver
import john.data.ConstraintUtil
import john.data.DivideUtil

/**
 * @author john
 */
object Main {
  def main(args: Array[String]): Unit = {
    val network = NQueens.createNetwork(10)
    //val divideMap = Divide(None, 5, None)
    val divideMap = DivideUtil.apply(5)
    //val divideMap = None
   // val value: Value = NoAssignment
 //   var solutions = Solver.solve(network)
//    println(solutions.size)

    val solutions = DivideAndConquer.DivCon(network, divideMap)
    val sol = solutions.solutions
    println(sol.head)

  } 
}