package john.data


/**
 * @author john
 */
abstract class DomainValue {
  def value: Any
}

case object NoValue

case object NoAssignment extends DomainValue {
  def value: Any = NoValue
}

case class Value(value: Any) extends DomainValue