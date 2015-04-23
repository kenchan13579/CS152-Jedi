package expressions

import values._
import ui._

case class Declaration( a : Identifier,  e : Expression) extends SpecialForm {
def execute( env : Environment):Value = { env.put(a, e.execute(env)); Notification.OK }
}