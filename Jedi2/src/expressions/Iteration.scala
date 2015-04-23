package expressions

import values._
import ui.TypeException
case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result: Value = Notification.UNKNOWN
    var foo = condition.execute(env)
    if (foo.isInstanceOf[Boole]) {
      
      while (condition.execute(env).toString()=="true") {
        result = body.execute(env)
      }
      result
    } else
      throw new TypeException("Type Exception error")

  }
}