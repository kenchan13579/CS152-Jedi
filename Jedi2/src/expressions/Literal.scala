package expressions
import values._
case class Literal() extends Expression with Value with Serializable{
def execute( env : Environment):Value = this
}