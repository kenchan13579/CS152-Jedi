package values

case class Notification(msg: String) extends Value{
def getMsg():String = msg
override def toString = msg
}

object Notification extends Value{
  val UNKNOWN = new Notification("unknown")
 val UPDATED  = new Notification("variable updated")
  val BINDING_CREATED = new Notification("Binding Created")
  var ERROR = new Notification("error")
  var OK = Notification("ok")
  var DONE = Notification("done")
  }
  
