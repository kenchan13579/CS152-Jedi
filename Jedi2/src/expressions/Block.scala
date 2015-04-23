package expressions

import values._

case class Block(locals: List[Expression]) extends SpecialForm {
def execute( env : Environment):Value = 
{
  var localEnv = new Environment(env)
  for ( i <- 0 until locals.length-1)
  {
    locals(i).execute(localEnv)
  }
  locals(locals.length-1).execute(localEnv)
}
}