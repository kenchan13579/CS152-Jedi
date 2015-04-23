package expressions
import values._
import ui._
import javax.management.Notification
case class Conjunction(exp: List[Expression]) extends SpecialForm {
 
    def execute(env: Environment): Value = {
    var result = true
    var i = 0
    for ( i <-0 until exp.length-1){
      if (exp(i).execute(env).isInstanceOf[Boole]) {
        result = exp(i).execute(env).asInstanceOf[Boole].value
        
      } else throw new JediException("Inputs  must be Booles")
    }
    new Boole(result)
  }
  
}