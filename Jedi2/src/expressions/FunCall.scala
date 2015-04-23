package expressions
import values._
import ui._
case class FunCall(op: Expression, args: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val useStatic: Boolean = false
     
    var env2 = env;
    if(!useStatic) env2 = null
      var temp = args.map(_.execute(env))
      try {
        if (op.execute(env).isInstanceOf[Closure]) op.execute(env).asInstanceOf[Closure].apply(temp,env2)
        else throw new UndefinedException
      } catch {
        case e: UndefinedException => system.execute(op.asInstanceOf[Identifier], temp)
      }
   
  }

}