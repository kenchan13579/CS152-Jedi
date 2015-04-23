package values
import expressions._
class Number(val value: Double) extends Literal with Value {
  def this(DoubleInString: String) { this(DoubleInString.toDouble) }
  def +(other: Number): Number = new Number(other.value + this.value)
  def -(other: Number): Number = new Number(this.value - other.value)
  def /(other: Number): Number = new Number(this.value / other.value)
  def *(other: Number): Number = new Number(this.value * other.value)
  def <(other: Number): Boole = if (this.value < other.value) new Boole(true) else new Boole(false)
  def ==(other: Number): Boole = if (this.value == other.value) new Boole(true) else new Boole(false)
  override def toString = value.toString
}

object Number {

  def test() {
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    println("Checking Number class:")
    println("...expected = " + (100.0 + 42.0) + ", actual = " + (num1 + num2))
    println("...expected = " + (100.0 * 42.0) + ", actual = " + (num1 * num2))
    println("...expected = " + (100.0 - 42.0) + ", actual = " + (num1 - num2))
    println("...expected = " + (100.0 / 42.0) + ", actual = " + (num1 / num2))
    println("...expected = " + (100.0 < 42.0) + ", actual = " + (num1 < num2))
    println("...expected = " + (100.0 == 42.0) + ", actual = " + (num1 == num2))
  }
}