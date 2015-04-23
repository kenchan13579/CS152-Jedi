package expressions


import values._
import ui.TypeException
case class Assignment(x : Identifier, update : Expression) extends SpecialForm {
 def execute( env : Environment):Value = {
  
 var foo = x.execute(env)
  if ( foo.isInstanceOf[Variable]){
    
    foo.asInstanceOf[Variable].content= update.execute(env)
   
       Notification.DONE 
  
  }
  else 
    throw new TypeException("Assignment must be Variable")
 }
}