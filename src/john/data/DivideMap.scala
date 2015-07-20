package john.data

/**
 * @author john
 */
sealed trait DivideMap

case object None extends DivideMap

case class Divide(mapLeft: DivideMap, index: Int, mapRight: DivideMap) extends DivideMap {
  
}

object DivideUtil{
  def apply(as: Int*): DivideMap = {
    if (as.isEmpty) None
    else Divide(None, as.head, apply(as.tail: _*))
  }
}