package john.solver.objects

import john.data.VariableAssignment
import john.data.Variable


/**
 * @author john
 */
abstract sealed class ConflictSet {
  def generators: Set[Generator]
  def stepAssigned: Int
  
  def contains(generator: Generator): Boolean = {
    generators.contains(generator)
  }
  
  def remove(generator: Generator, stepCount: Int): ConflictSet = {
    val newGenerators = generators - generator
    if (newGenerators.size > 0) {
      return Conflicts(newGenerators, stepCount)
    }
    NoConflict
  }
  
  def union(cs: ConflictSet): ConflictSet = (this, cs) match {
    case (_, NoConflict) => this  
    case (NoConflict, _) => cs
    case (Conflicts(gen1, sa1), Conflicts(gen2, sa2)) => Conflicts(gen1 ++ gen2, Math.max(sa1, sa2))
  }
}

case object NoConflict extends ConflictSet {
  def generators: Set[Generator] = Set[Generator]()
  def stepAssigned: Int = -1
}

case class Conflicts(generators: Set[Generator], stepAssigned: Int) extends ConflictSet

object ConflictUtils {
  
}


