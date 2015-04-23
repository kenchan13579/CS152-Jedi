package expressions
import values._
import ui._

import ui.UndefinedException
import javax.management.Notification
case class Identifier(name: String) extends Expression with Serializable{
def execute( env : Environment):Value = {
  if (env.find(this)==values.Notification.UNKNOWN ) throw new UndefinedException("Undefined Identifier: "+name)
  
  else env.find(this)
}
}