package john.queens

import john.data.Network
import john.queens.data.BoardPosition
import john.data.Value
import john.data.Variable
import john.data.Constraint
import john.data.Solution
import john.data.VariableAssignment


/**
 * @author john
 */
object NQueens {
  
  def createNetwork(n: Int): Network = {
    val variables = createVariables(n)
    val constraints = createConstraints(n, variables)
    
    Network(variables, constraints, 0)    
  }
  
  def createVariables(n: Int): Array[Variable] = {
    val variables = for (i <- 0 until n) yield createVariable(n, i)
    return variables.toArray
  }
  
  def createVariable(n: Int, xcoord: Int): Variable = {
    val domain = for (i <- 0 until n) yield Value(BoardPosition(xcoord, i))
    new Variable(domain.toArray)
  }
  
  def createConstraints(n: Int, variables: Array[Variable]): Set[Constraint] = {
    val constraints = for (i <- 0 until n; j <- (i+1) until n) yield createConstraint(n, i, j, variables) 
    
    return constraints.toSet
  }
  
  def createConstraint(n: Int, i: Int, j: Int, v: Array[Variable]): Constraint = {
    val variables = Array[Variable](v(i), v(j))
    
    val table = for (k <- 0 until n; l <- 0 until n if(conditions(i,j,k,l))) yield createSolution(i,j,k,l,v)
    
    return Constraint(variables, table.toSet)
  }
  
  def conditions(i: Int, j: Int, k: Int, l: Int): Boolean = {
    if (k == l) return false
    
    if ((j-i) == Math.abs((k-l))) return false
    return true
  }
  
  def createSolution(i: Int, j: Int, k: Int, l: Int, v: Array[Variable]): Solution = {
    val var1 = VariableAssignment(v(i), Value(BoardPosition(i,k)))
    val var2 = VariableAssignment(v(j), Value(BoardPosition(j,l)))
    Solution(Vector[VariableAssignment](var1, var2))
  }
}