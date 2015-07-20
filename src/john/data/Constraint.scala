package john.data

/**
 * @author john
 */
case class Constraint(variables: Array[Variable], table: Set[Solution]) 

object ConstraintUtil {
  var count = 0
  
  def satisfiesConstraints(constraints: Set[Constraint], solution: Solution): Boolean = {
    for(constraint <- constraints) {
      val partialAssignment = solution.solution.filter(a => constraint.variables.contains(a.variable))
      if (!satisfiesConstraint(constraint, Solution(partialAssignment))) return false
    }
    true
  }
  
  def satisfiesConstraint(constraint: Constraint, assignment: Solution): Boolean = {   
    constraint.table.contains(assignment)
  }
  
  def increment(): Unit = {
    count = count + 1
  }
}