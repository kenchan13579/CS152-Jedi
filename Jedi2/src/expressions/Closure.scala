package expressions
import values._
import ui._
class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
   def apply(args: List[Value] , callEnv: Environment = null): Value = {
     var tempEnv = new Environment()
    if(callEnv == null) tempEnv = new Environment(defEnv)
    else tempEnv = callEnv
    
     if (args.length==params.length){
       
       tempEnv.put(params, args)
        body.execute(tempEnv)
     }
     else throw new TypeException
    
   }
}

