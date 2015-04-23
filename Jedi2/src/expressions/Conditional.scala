package expressions

import values._

case class Conditional(exp1: Expression, exp2: Expression, exp3: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value ={
    if ( exp1.execute(env).toString()=="true") exp2.execute(env)
    else if (exp1.execute(env).toString()=="false") exp3. execute(env)  
    else Notification.UNKNOWN 
  }
    
}