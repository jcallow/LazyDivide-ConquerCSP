package john.solver.objects

/**
 * @author john
 */

abstract sealed class Value {
  val value: Any
  var conflicts: ConflictSet
  
  def setConflictSet(conflicts: ConflictSet) = {
    this.conflicts = conflicts
  }
  
  def fail(m: String) = throw new NoSuchElementException(m)
}

case object NoValue

case object NoAssignment extends Value {
  val value: Any = NoValue
  var conflicts: ConflictSet = NoConflict
}

case class Assignment(value: Any, var conflicts: ConflictSet = NoConflict) extends Value 