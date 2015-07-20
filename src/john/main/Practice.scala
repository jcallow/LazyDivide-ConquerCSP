package john.main

import john.data.Value
import john.data.DomainValue
import john.data.Variable
import john.data.Variable
import john.data.VariableAssignment
import john.data.Solution

/**
 * @author john
 */
object Practice {
  def main(args: Array[String]): Unit = {
    val domain = Value(1)
    val var1 = new Variable(Array[DomainValue](domain))
    val var2 = new Variable(Array[DomainValue](domain))
    
    println(var1 == var2)
    
    val ass1 = VariableAssignment(var1, domain)
    val ass2 = VariableAssignment(var1, domain)
    
    val sol1 = Solution(Vector[VariableAssignment](ass1, ass2))
    val sol2 = Solution(Vector[VariableAssignment](ass1, ass2))
    
    val set = Set[Solution](sol1)
    
    println(sol1 == sol2)
    println(set.contains(sol2))
  }
}