package john.divideconquer

import john.data.DivideMap
import john.data.Divide
import john.data.None
import john.data.Variable
import john.data.Constraint
import john.data.Network
import john.data.SolutionSet
import john.data.NetworkMap
import john.data.Solution
import john.data.ConstraintUtil
import john.solver.Solver._

/**
 * @author john
 * My attempt to recreate a lazy divide and conquer algorithm for csp in scala.  Original paper at: 
 */

object DivideAndConquer {
  def DivCon(n: Network, p: DivideMap): SolutionSet = {
    if (primitiveMap(p)) return solve(n)
    val divided = divide(n, p.asInstanceOf[Divide])
    val nms = divided._2
    val C = divided._1
    
    val phi = for (nm <- nms) yield DivCon(nm.n, nm.p)
    val answer = join(C, phi)
    return answer
  }
  
  def primitiveMap(p: DivideMap): Boolean = p match {
    case None => true
    case _ => false 
  }
  
  def divide(n: Network, p: Divide): (Set[Constraint], Stream[NetworkMap]) = {
    val splitIndex = p.index - n.index
    val constraints = n.constraints
    
    val varL = n.variables.slice(0, splitIndex + 1)
    val constraintsL = constraints.filter(c => includeConstraint(varL, c))
    val networkL = Network(varL, constraintsL, n.index)
    val networkMapL = NetworkMap(networkL, p.mapLeft)
    val varR = n.variables.slice(splitIndex, n.variables.size)
    val constraintsR = constraints.filter(c => includeConstraint(varR, c))
    val networkR = Network(varR, constraintsR, p.index)
    val networkMapR = NetworkMap(networkR, p.mapRight)

    val LUR = constraintsL ++ constraintsR
    val crossConstraints = constraints -- LUR
    
    return (crossConstraints, Stream[NetworkMap](networkMapL, networkMapR))
  }
  
  def includeConstraint(variables: Array[Variable], c: Constraint): Boolean = {
    for (v <- c.variables) {
      if (!variables.contains(v)) return false
    }
    true
  }
    
  def join(C: Set[Constraint], phi: Stream[SolutionSet]): SolutionSet = {
    val lss = phi.head.solutions
    val rss = phi.tail.head.solutions
//    println(lss.size)
//    println(rss.size)
    var combinedSolutions = List[Solution]()
    
    for (solutionsr <- rss) {
      for (solutionsl <- lss) {
        
        val solutionL = solutionsl.solution
        val solutionR = solutionsr.solution
        if (solutionL(solutionL.size-1) == solutionR.head) {
          
          val left = solutionL
          val right = solutionR.slice(1, solutionR.size)
          val combined = Solution(left ++ right)
          if (ConstraintUtil.satisfiesConstraints(C, combined)) {
            combinedSolutions =  combined :: combinedSolutions
          }
        }
      }  
    }
    return SolutionSet(combinedSolutions.toStream)
  }
}