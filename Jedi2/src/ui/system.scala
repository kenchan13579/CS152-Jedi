package ui
import expressions._
import values._
import values._

object system {
 def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" =>  mul(args)
      case "div" => div(args)
      case "sub" =>sub(args)
      case "equals" => equal(args)
      case "less" =>less(args)
      case "not" => not(args)
      case "great" => great(args)
      case "content" =>setContent(args)
      case "var" => makeVar(args)
      // mul, sub, div, equals, less, not,etc.
      case _ => throw new UndefinedException(opcode.name)
    }
  }
//def exp = lambda (x,y) {def count = var(0); def result = var(0);while ( [count] < y) result= add([result], mul(x,x));[result]}
 private def setContent( args : List[Value]):Value = {
   if (args.isEmpty) throw new TypeException("error")
    if ( args.head.isInstanceOf[Variable]) args.head.asInstanceOf[Variable].content 
    else throw new TypeException
 } // args.head.content
 private def makeVar(args : List [ Value]) = {
   if (args.isEmpty) throw new TypeException("error")
   new Variable(args.head)
 } // new Variable (argss.head)
  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_+_)
  }
  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_-_)
  }
  private def mul(vals : List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_*_)
  }
  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
       args2.reduce(_/_)
  }
  private def equal(vals : List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException("equal check expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
   var isEqual = true
   for ( i <- 1 until args2.length)
   {
     if (args2(0).value!=args2(i).value) isEqual = false
   }
      if (isEqual) new Boole(true) else new Boole(false)
  }
  private def not(vals : List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException("expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("inputs must be Boolean")
      if (vals.length>1) throw new TypeException(" Too many arguements")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2(0)!
  }
    private def less(vals : List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException(" expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("input must be number")
    val args2 = vals.map(_.asInstanceOf[Number])
    var isLess = true
    
    for ( i <- 1 until args2.length){
      if (args2(i-1).value >=args2(i).value) isLess=false
    }
    if (isLess) new Boole(true) else new Boole(false)
  }
     private def great(vals : List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException(" expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("input must be number")
    val args2 = vals.map(_.asInstanceOf[Number])
    var isGreat = true
    
    for ( i <- 1 until args2.length){
      if (args2(i-1).value <=args2(i).value) isGreat=false
    }
    if (isGreat) new Boole(true) else new Boole(false)
  }

 def test() {
   val globalEnv = new Environment()
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    val bool1 = new Boole(true)
    val id1 = new Identifier("x")
    val id2 = new Identifier("y")
    val id3 = new Identifier("z")
    globalEnv.put(List(id1, id2, id3), List(num1, num2, bool1))
    println("... expected = 100.0, actual = " + id1.execute(globalEnv))
    println("... expected = 42.0, actual = " + id2.execute(globalEnv))
    println("... expected = true, actual = " + id3.execute(globalEnv))
    
   // next, create & execute some funcalls:
    
    // executing add(x, 42)
    var op = new Identifier("add")
    var args = List(id1, num2)
    var fc1 = new FunCall(op, args)
    println("... expected = 142.0, actual = " + fc1.execute(globalEnv))
    
   // executing add(x, 42, add(x, 42))
    var op2 = new Identifier("add")
    var args2 = List(id1, num2, fc1)
    var fc2 = new FunCall(op2, args2)
    println("... expected = 284.0, actual = " + fc2.execute(globalEnv))
    
   // executing sub(x, 42)
    op = new Identifier("sub")
    fc1 = new FunCall(op, args)
    println("... expected = 58.0, actual = " + fc1.execute(globalEnv))
    
    // executing mul(x, 42)
    op = new Identifier("mul")
    fc1 = new FunCall(op, args)
    println("... expected = 4200.0, actual = " + fc1.execute(globalEnv))
    
    // executing div(x, 42)
    op = new Identifier("div")
    fc1 = new FunCall(op, args)
    println("... expected = 2.380952..., actual = " + fc1.execute(globalEnv))
    
    // executing equals(x, 42)
    op = new Identifier("equals")
    fc1 = new FunCall(op, args)
    println("... expected = false, actual = " + fc1.execute(globalEnv))
   var args5 = List(id1,id2,id3)
   var fc3 =  new FunCall(op, args5)
   println("... expected = false, actual = " + fc3.execute(globalEnv))
    // executing less(x, 42)
    op = new Identifier("less")
    fc1 = new FunCall(op, args)
    println("... expected = false, actual = " + fc1.execute(globalEnv))
    
    // executing not(less(x, 42))
    op2 = new Identifier("not")
    args2 = List(fc1)
    fc2 = new FunCall(op2, args2)
    println("... expected = true, actual = " + fc2.execute(globalEnv))
    
 }
  // etc.
}