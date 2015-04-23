package expressions

import values._
import ui._
case class Disjunction( exp : List[Expression])extends SpecialForm {

   def execute(env: Environment): Value = {
    var isOK = false
  
    for ( i <- 0 until  exp.length-1 )
   {
      if (exp(i).execute(env).isInstanceOf[Boole]) {
        isOK =true
       
      } else throw new JediException("Inputs must be Booles")
    }
    new Boole(isOK)
  }
}