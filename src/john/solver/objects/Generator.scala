package john.solver.objects

import john.data.Constraint
import scala.collection.mutable.ArrayBuffer
import john.data.Variable

/**
 * @author john
 */
class Generator(val filters: Array[Constraint],val variable: Variable) {
  var currentIndex = 0
  val assignOrder = ArrayBuffer[Value]((variable.domain.map(d => Assignment(d.value))) : _*)
  var currentAssignment:Value = Assignment(variable.domain(0), NoConflict)
  var stepCount = 0
  
  def resetCount(): Unit = {
    currentIndex = 0
  }
  
  def setWorkingHypothesis = {
    val index = assignOrder.indexOf(currentAssignment)
    val oldValue = assignOrder(0)
    assignOrder(0) = currentAssignment
    assignOrder(index) = oldValue
  }
  
  // This can be sped up by a divide and conquer approach.  Not really necessary for this though.
  def getConflictSets(): ConflictSet = {
    val conflictSets = assignOrder.map(a => a.conflicts)
    
    return conflictSets.reduce((a,b) => a.union(b))
  }
  
  def getNext(): Value = {
    if (currentIndex < assignOrder.size) {
      val value = assignOrder(currentIndex)
      currentIndex += 1
      return value
    } else {
      return NoAssignment
    }
  }  
}