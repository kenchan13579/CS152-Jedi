package ui

import values. _
import expressions. _

object codeChecker {
  
  def check2() {
    
    val globalEnv = new Environment()
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    val bool1 = new Boole(true)
    
    // executing literals: 100, 42, true
    println("... expected = 100.0, actual = " + num1.execute(globalEnv))
    println("... expected = 42.0, actual = " + num2.execute(globalEnv))
    println("... expected = true, actual = " + bool1.execute(globalEnv))
    
  // put some stuff in global environment
    val id1 = new Identifier("x")
    val id2 = new Identifier("y")
    val id3 = new Identifier("z")
    globalEnv.put(List(id1, id2, id3), List(num1, num2, bool1))
    
    // executing identifiers: x, y, z
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
   
    // executing less(x, 42)
    op = new Identifier("less")
    fc1 = new FunCall(op, args)
    println("... expected = false, actual = " + fc1.execute(globalEnv))
    
    // executing not(less(x, 42))
    op2 = new Identifier("not")
    args2 = List(fc1)
    fc2 = new FunCall(op2, args2)
    println("... expected = true, actual = " + fc2.execute(globalEnv))
    op = new Identifier("equals")
     var args5 = List(id1,id1,id2)
   var fc3 =  new FunCall(op, args5)
   println("#1... expected = false, actual = " + fc3.execute(globalEnv))
    var args6 = List(id1,id1,id1)
   var fc4 =  new FunCall(op, args6)
   println("#2... expected = true, actual = " + fc4.execute(globalEnv))
    op = new Identifier("less")
    var args7 = List(new Number(10),new Number(11),new Number(12),new Number(13))
    var args8 = List(new Number(10),new Number(12),new Number(11),new Number(13))
    var fc5 = new FunCall(op,args7)
    var fc6  = new FunCall(op, args8)
       println("#3 less... expected = true, actual = " + fc5.execute(globalEnv))
     println("#4 less... expected = false, actual = " + fc6.execute(globalEnv))
  }
  def check() {
    println("Checking Number class:")
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    println("...expected = " + (100.0 + 42.0) + ", actual = " + (num1 + num2) )
    println("...expected = " + (100.0 * 42.0) + ", actual = " + (num1 * num2))
    println("...expected = " + (100.0 - 42.0) + ", actual = " + (num1 - num2))
    println("...expected = " + (100.0 / 42.0) + ", actual = " + (num1 / num2))
    println("...expected = " + (100.0 < 42.0) + ", actual = " + (num1 < num2))
    println("...expected = " + (100.0 == 42.0) + ", actual = " + (num1 == num2))
    
    println("Checking Boole class:")
    val t = new Boole(true)
    val f = new Boole(false)
    println("...expected = " + (true && false) + ", actual = " + (t && f))
    println("...expected = " + (true && true) + ", actual = " + (t && t))
    println("...expected = " + (true || false) + ", actual = " + (t || f))
    println("...expected = " + (false || false) + ", actual = " + (f && f))
    
   println("Checking Environment class:")
    val n1 = new Identifier("n1")
    val n2 = new Identifier("n2")
    val n3 = new Identifier("n3")
    val n4 = new Identifier("n4")
    val b1 = new Identifier("b1")
    val b2 = new Identifier("b2")
    val b3 = new Identifier("b3")
    val b4 = new Identifier("b4")
    val x = new Identifier("x")
    val globalEnv = new Environment()
    val env1 = new Environment(globalEnv)
    val env2 = new Environment(env1)
    globalEnv.put(List(n1, n2, b1, b2), List(num1, num2, t, f))
    env1.put(List(n3, b3), List(num1 + num2, num1 < num2)) 
   env2.put(List(n2, n4, b3, b4), List(num2 - num1, num1 / num2, num2 < num1, f))
    
    println("...expected = " + num1 + " actual = " + env2.find(n1))
    println("...expected = " + (num2 - num1) + " actual = " + env2.find(n2))
    println("...expected = " + (num2 + num1) + " actual = " + env2.find(n3))
    println("...expected = " + (num1 / num2) + " actual = " + env2.find(n4))
    println("...expected = " + (t) + " actual = " + env2.find(b1))
    println("...expected = " + (f) + " actual = " + env2.find(b2))
    println("...expected = " + (num2 < num1) + " actual = " + env2.find(b3))
    println("...expected = " + (f) + " actual = " + env2.find(b4))
    println("...expected = " + Notification.UNKNOWN + " actual = " + env2.find(x))
    
    println("Checking polymorphism:")
    var exp: Expression = num1
    println("...exp = " + exp)
    exp = t
    println("...exp = " + exp)
    exp = n1
    println("...exp = " + exp)
    var value: Value = num1
    println("...value = " + value)
    value = t
    println("...value = " + value)
    value = env2
    println("...value = " + value)    
  }

  def main(args: Array[String]): Unit = { check2(); }

}