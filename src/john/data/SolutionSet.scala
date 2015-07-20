package john.data

/**
 * @author john
 */
case class SolutionSet(solutions: Array[Solution]) { 
  def addSolution(solution: Solution): SolutionSet = {
    SolutionSet(solutions :+ solution)
  }
  
  def size(): Int = {
    solutions.size
  }
}