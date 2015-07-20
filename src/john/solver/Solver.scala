package john.solver

import john.data.Network
import john.data.SolutionSet
import john.data.Solution
import john.solver.objects.ConflictSet
import john.solver.objects.Generator
import john.data.DomainValue
import john.solver.objects.NoConflict
import john.data.VariableAssignment
import john.data.Constraint
import john.solver.objects.Value
import john.solver.objects.NoAssignment
import john.data.Variable
import john.data.ConstraintUtil
import john.solver.objects.Conflicts
import john.data.{Value => dataValue}
import john.solver.objects.FoundSolution

/**
 * @author john
 */
class CSPSolver(generators: Array[Generator], assignmentOrder: Array[Variable], desiredSolutions: Int = -1) {
  var stepCount: Int = 0
  val MAX_DEPTH = assignmentOrder.size - 1
  val generatorMap = generators.map(g => g.variable -> g).toMap
  
  def solve(): SolutionSet = {
    val solutions:Stream[Solution] = begin()
    return SolutionSet(solutions)
  }
    
  def begin(): Stream[Solution] = {
    val conflicts = extendAssignment(0, false)
    if (conflicts == FoundSolution) {
      val solution = recordSolution()
      return solution #:: continue()
    } else {
      return Stream.empty[Solution]
    }
  }
  
  def continue(): Stream[Solution] = {
    val conflicts = extendAssignment(0, true)
    if (conflicts == FoundSolution) {
      val solution = recordSolution()
      return solution #:: continue()
    } else {
      return Stream.empty[Solution]
    }
  }
  
  def extendAssignment(currentDepth: Int, toTheTop: Boolean): ConflictSet = {
    
    val generator = generators(currentDepth)
    var conflictSet: ConflictSet = NoConflict
    
    if (toTheTop && currentDepth != MAX_DEPTH) { 
      conflictSet = extendAssignment(currentDepth + 1, toTheTop)
        
      if (conflictSet == FoundSolution) return FoundSolution
      
      if(!conflictSet.contains(generator) && conflictSet != NoConflict) {
        generator.setWorkingHypothesis
        return conflictSet
      }

      conflictSet = conflictSet.remove(generator, stepCount)
      generator.currentAssignment.setConflictSet(conflictSet)
    } else if (!toTheTop){
      generator.resetCount
    }
    
    while (assignVariable(generator, currentDepth)) {
      stepCount += 1
      
      if (currentDepth == MAX_DEPTH) {
        return FoundSolution
      } else {
        
        conflictSet = extendAssignment(currentDepth + 1, false)
        
        if (conflictSet == FoundSolution) return FoundSolution
      
        if(!conflictSet.contains(generator) && conflictSet != NoConflict) {
          generator.setWorkingHypothesis
          return conflictSet
        }
      }
      conflictSet = conflictSet.remove(generator, stepCount)
      generator.currentAssignment.setConflictSet(conflictSet)
      
    }
    
    return generator.getConflictSets
  }
  
  def assignVariable(generator: Generator, currentDepth: Int): Boolean = {
    var assignment: Value = NoAssignment
    var conflictSet: ConflictSet = NoConflict
    
    while({
      assignment = selectNextAssignment(generator)
      assignment != NoAssignment
    }) {
      conflictSet = filterCurrentAssignment(generator)
      
      if(conflictSet == NoConflict) {
        assignment.conflicts = NoConflict
        return true
      } else {
        assignment.conflicts = conflictSet
      }
    }
    return false    
  }
  
  def selectNextAssignment(generator: Generator): Value =  {
    var assignment: Value = NoAssignment
    while({
      assignment = generator.getNext()
      assignment != NoAssignment
    }) {
      if (recentlyReassigned(assignment.conflicts)) {
        generator.currentAssignment = assignment
        generator.stepCount = stepCount
        return assignment
      }
    }
    return NoAssignment
  }
  
  def recentlyReassigned(conflictSet: ConflictSet): Boolean = {
    if (conflictSet == NoConflict) return true
    for (generator <- conflictSet.generators) {
      if (generator.stepCount >= conflictSet.stepAssigned) return true
    }
    return false
  }
  
  def filterCurrentAssignment(generator: Generator): ConflictSet = {
    val filters = generator.filters
    val variable = generator.variable
    var conflictingGenerators: Set[Generator] = Set[Generator]()
     
    for (constraint <- filters) {
      val index = constraint.variables.indexOf(variable)
      val variables = constraint.variables.slice(0, index+1)
      
      
      val currentAssignments = variables.map(v => generators(assignmentOrder.indexOf(v)).currentAssignment)
      val partialAssignment = Solution(variables.zip(currentAssignments)
          .map(v => VariableAssignment(v._1,john.data.Value(v._2.value))).toVector)   

      if (!ConstraintUtil.satisfiesConstraint(constraint, partialAssignment)) {
        conflictingGenerators = conflictingGenerators ++ constraint.variables.map(v => generatorMap(v)).toSet
      }
    }
    if (conflictingGenerators.size == 0) return NoConflict
    return Conflicts(conflictingGenerators, stepCount)
  }
  
  def recordSolution(): Solution = {
    val assignments = generators.map(g => VariableAssignment(g.variable, dataValue(g.currentAssignment.value))).toVector
    val solution = Solution(assignments)
    solution
  }
}

object Solver {
  def solve(n: Network): SolutionSet = {
    val filters = n.constraints
    val variables = n.variables
    val filterMap = filters.flatMap(c => (c.variables.map(v => (v,c))))
          .filter(vc => {
            val index = variables.indexOf(vc._1)
            val assignedVariables = variables.slice(0, index+1)
            val constraintVariables = vc._2.variables          
            val keep = constraintVariables.toList.map(v => assignedVariables.contains(v)).reduce(_ && _)
            keep
          })
        .groupBy(_._1)
        .map{case (k,v) => (k,v.map(_._2) )}
    
    val generators = variables.map(v => new Generator(if (filterMap.contains(v)) filterMap(v).toArray else Array[Constraint](), v))
    val solver = new CSPSolver(generators, variables)    
    
    return solver.solve
  }
}