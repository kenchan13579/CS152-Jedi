package values
import expressions._

class Boole(val value : Boolean) extends Literal with Value{
  def this( booleanInString : String){ this(booleanInString.toBoolean)}
def ||(other : Boole):Boole =  if (this.value || other.value) new Boole(true) else new Boole(false)
def &&(other : Boole):Boole= if (this.value && other.value) new Boole(true) else new Boole(false)
def !():Boole =  if (this.value) new Boole(false) else new Boole(true)
override def toString() = value.toString

}

object Boole {
  def test(){
     val t = new Boole(true)
    val f = new Boole(false)
      println("Checking Boole class:")
    println("...expected = " + (true && false) + ", actual = " + (t && f))
    println("...expect = " + !true + ", actual = " + (t!))
    println("...expected = " + (true && true) + ", actual = " + (t && t))
    println("...expected = " + (true || false) + ", actual = " + (t || f))
    println("...expected = " + (false || false) + ", actual = " + (f && f))
  }
}