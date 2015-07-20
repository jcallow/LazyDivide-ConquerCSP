package john.data

/**
 * @author john
 */
case class SolutionSet(solutions: Stream[Solution]) { 
  def addSolution(solution: Solution): SolutionSet = {
    SolutionSet(solutions :+ solution)
  }
  
  def size(): Int = {
    solutions.size
  }
}