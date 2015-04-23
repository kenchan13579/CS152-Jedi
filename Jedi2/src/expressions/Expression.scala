package expressions
import values._
trait Expression {
def execute( env : Environment):Value
}