package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Evan Dorrough>
   * 
   * Partner: <Sabrina Kavesh>
   * Collaborators: <Nick Lopinski>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      //takes a true/false and turns into a number
      case B(b) => if (b == true) 1 else 0 // takes a true/false and turns into a number//test case in lab2spec at bottom named 'toNumber'
      case S("") => 0
      case S(s) => try s.toDouble catch {
        case _: Throwable => Double.NaN//returns not a number if given a word
      }

      //}
      case Undefined => Double.NaN
      //      case _ => Double.NaN

      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = { //takes a bool,string,numb and turns into a bool
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => {
        if (n > 0)  true //as long as it is not 0 or -0, true. 0 and -0 return false
        else if (n < 0) true
        else false
      }
      case S(s) => { //strings are always true
        if (s == "") false
        else true
      }
      case Undefined => {
        false
      }
      //case _ =>
      // false
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = { //converts a number,bool, or string into a string
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n) => {
        n.toString()
      }
      //case B(b) => "" + b + ""  //adds quotes around it to make it a string
      case B(b) => {
        b.toString()
      } //toString in ast file
      case Undefined => "undefined"

      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {  //environment is first value in each eval
      /* Base Cases */
      case N(n) => e  //e is input value
      case S(s) => e
      case B(b) => e
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case ConstDecl(x,e1,e2) => {
        eval(extend(env, x, eval(env,e1)), e2) //eliminates need for new var,
        // gets same score as two lines below
        // val t1 = extend(env, x, eval(env, e1))
        //eval(t1,e2)
      }
      case Var(x) => {  //defines environment (?)
        lookup(env,x)
      }

      case Unary(Neg,e1) =>
        N(-toNumber(eval(env,e1)))  //**this line gives same score as having all below

      /*(eval(env,e1)) match {
          case (N(n1)) => N(-n1)
          //case (S(_)) => N(toNumber(e1) * -1.0) //not a number, so handled in _ case
          case (B(b1)) => N(toNumber(B(b1)) * -1.0)
          case _ => N(toNumber(e1) * -1.0)

          //case Undefined => Undefined //not necessary

        } */// N(toNumber(eval(env, e1)) * toNumber(eval(env,e2)))

      case Unary(Not,e1) => //(eval(env,e1)) match { //return compliment of bool
        // case (B(b1)) => B(!b1)  //! undf,0,false,"" == true, !true,string,# = false
        //case (S(_)) => B(!toBoolean(e1)) always false for not
        // case (N(n1)) => B(!toBoolean(N(n1))) //always false unless 0
        B(!toBoolean(eval(env,e1)))
      //case Undefined => Undefined //covered in blank case
      // case _ => B(!toBoolean(eval(env,e1)))
      //}


      case Binary(Plus,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        // case (N(n1), N(n2)) => N(n1 + n2) //N(toNumber(eval(env,N(e1))) + toNumber(eval(env,N(e2))))
        //          case (B(b1), B(b2)) => N(toNumber(B(b1)) + toNumber(B(b2)))//N(toNumber(eval(env,B(e1))) + toNumber(eval(env,B(e2))))
        case (S(s1), S(s2)) => S(s1 + s2)//S(toStr(eval(env,S(e1))) + toStr(eval(env,S(e2))))//S(toStr(eval(env,S(e1))) + toStr(eval(env,S(e2))))
        case (S(s1),_) => S(s1 + toStr(e2))
        case (_,S(s2)) => S(toStr(e1) + s2) //these ^3 cases and blank at bottom get same score as all others
        //          case (B(b1),N(n2)) => N(toNumber(B(b1)) + n2)//N(toNumber(eval(env,B(e1))) + toNumber(eval(env,N(e2))))
        //          case (N(n1),B(b2)) => N(n1 + toNumber(B(b2)))//N(toNumber(eval(env,N(e1))) + toNumber(eval(env,B(e2))))
        /* case (S(s1),N(n2)) => S(s1 + toStr(N(n2)))
         case (N(n1),S(s2)) => S(toStr(N(n1)) + s2)
         case (S(s1),B(b2)) => S(s1 + toStr(B(b2)))
         case (B(b1),S(s2)) => S(toStr(B(b1)) + s2)*/
        case _ => N(toNumber(eval(env,e1)) + toNumber(eval(env,e2)))
      }
      case Binary(Minus,e1,e2) =>
        N(toNumber(eval(env,e1)) - toNumber(eval(env,e2))) //nn, bb, bn, nb //better score than a combo of lines below

      /*(eval(env,e1), eval(env,e2)) match {
        //case (N(e1), N(e2)) => N(toNumber(eval(env, N(e1))) - toNumber(eval(env, N(e2))))
        case (B(e1),N(e2)) => N(toNumber(eval(env,B(e1))) - toNumber(eval(env,N(e2))))
        case (N(e1),B(e2)) => N(toNumber(eval(env,N(e1))) - toNumber(eval(env,B(e2))))
        case (N(n1), N(n2)) => N(n1 - n2) //N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))
        case (B(b1), B(b2)) => N(toNumber(B(b1)) - toNumber(B(b2)))*/ //N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))

      // case (S(_) , S(_)) => N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))
      // case _ => N(Double.NaN)//N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))


      case Binary(Times,e1,e2) => //(eval(env,e1), eval(env,e2)) match {
        //        case (N(_), N(_)) => N(toNumber(eval(env,e1)) * toNumber(eval(env,e2)))
        //        case (B(_), B(_)) => N(toNumber(eval(env,e1)) * toNumber(eval(env,e2)))
        //        case (S(_) , S(_)) => N(toNumber(eval(env,e1)) * toNumber(eval(env,e2)))
        N(toNumber(eval(env, e1)) * toNumber(eval(env,e2)))  //returns same type always

      //}
      case Binary(Div,e1,e2) => //(eval(env,e1), eval(env,e2)) match {
        //        case (N(_), N(_)) => require(e2 != 0)
        //          N(toNumber(eval(env,e1)) / toNumber(eval(env,e2)))
        //        case (B(_), B(_)) => require(e2 != 0)
        //          N(toNumber(eval(env,e1)) / toNumber(eval(env,e2)))
        //        case (S(_) , S(_)) => require(e2 != 0)
        //          N(toNumber(eval(env,e1)) / toNumber(eval(env,e2)))
        //require(e2 != 0)
        N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
      //}


      //}
      case Binary(Eq,e1,e2) => //(eval(env,e1), eval(env,e2)) match {  //already calling eval here, so dont need to below
        B(eval(env,e1) == eval(env,e2))

      //case (N(n1), N(n2)) => B(n1 == n2) //B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
      //case (S(s1), S(s2)) => B(s1 == s2) //B(toStr(eval(env,e1)) == toStr(eval(env,e2)))
      //case (B(b1), B(b2)) => B(b1 == b2) //B(toBoolean(eval(env,e1)) == toBoolean(eval(env,e2)))
      //
      // B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
      //  }
      case Binary(Ne,e1,e2) => //(eval(env,e1), eval(env,e2)) match {  // dif combos in node
        // case (N(_),N(_)) => B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))
        //case (B(_),B(_)) => B(e1 != e2)//B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
        //case (S(_),S(_)) => B(toBoolean(eval(env,e1)) != toBoolean(eval(env,e2)))
        // case (N(n1), N(n2)) => B(n1 != n2) //already eval at top//B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))
        //case (S(s1), S(s2)) => B(s1 != s2) //B(toStr(eval(env,e1)) != toStr(eval(env,e2)))
        // case (B(b1), B(b2)) => B(b1 != b2) //B(toBoolean(eval(env,e1)) != toBoolean(eval(env,e2)))
        B(eval(env,e1) != eval(env,e2))  //HIGHER score than using only s()s() and _ cases
      //case _ => B(toNumber(eval(env,e1)) != toNumber(eval(env,e2))) //B(true) //B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))
      //}
      case Binary(Lt,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        /*case (N(_),N(_)) => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))
        case (B(_),B(_)) => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))
        case (S(_),S(_)) => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))*/

        //case (N(n1), N(n2)) => B(n1 < n2)
        case (S(s1), S(s2)) => B(s1 < s2)
        //case (B(b1), B(b2)) => B(b1 < b2)
        //case (B(b1), N(n2)) => B(toNumber(B(b1)) < n2)
        case _ => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))
      }
      case Binary(Le,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        /*case (N(_),N(_)) => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))
        case (B(_),B(_)) => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))//B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
        case (S(_),S(_)) => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))*/

        //case (N(n1), N(n2)) => B(n1 <= n2)
        case (S(s1), S(s2)) => B(s1 <= s2)
        //case (B(b1), B(b2)) => B(b1 <= b2)
        //
        case _ => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))
      }
      case Binary(Gt,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        /* case (N(_),N(_)) => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))
         case (B(_),B(_)) => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))//B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
         case (S(_),S(_)) => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))*/
        // case (N(n1), N(n2)) => B(n1 > n2)
        case (S(s1), S(s2)) => B(s1 > s2)
        //case (B(b1), B(b2)) => B(b1 > b2)
        //
        case _ => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))
      }
      case Binary(Ge,e1,e2) => (eval(env,e1), eval(env,e2)) match {  //test num and str
        /*case (N(_),N(_)) => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))
        case (B(_),B(_)) => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))//B(toNumber(eval(env,e1)) == toNumber(eval(env,e2)))
        case (S(_),S(_)) => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))*/
        //case (N(n1), N(n2)) => B(n1 >= n2)
        case (S(s1), S(s2)) => B(s1 >= s2) //case for string comparisons
        //case (B(b1), B(b2)) => B(b1 >= b2)
        //
        case _ => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))
      }


      case Binary(And,e1,e2) => {
        if(toBoolean(eval(env,e1))){  //checks if e1 is true, then returns result of e2
          eval(env,e2) //if e1 true, whatever e2 is is the ansewr
          //B(toBoolean(eval(env,e2))) //bc e2 is either true or false and so is correct result
        }
        else eval(env,e1) //e1 is false and so returns false
      } //if(e1 == e2)

      case Binary(Or,e1,e2) => {
        if(toBoolean(eval(env,e1))) eval(env,e1) //if e1 true, statement true
        else eval(env,e2) //if e1 false, whatever e2 is is correct answer
        /* if (toBoolean(eval(env,e1))) eval(env,e1)  //if statements and booleans auto checks if true//turns e1 or e2 into bools and if true returns e1 bc true
         else if (toBoolean(eval(env,e2))) eval(env,e2) //same as above but with e2
         else  eval(env,e1) //if neither are true returns false*/
      }

      case Binary(Seq,e1,e2) => {
        eval(env,e1)
        eval(env,e2)
      }

      case If(e1,e2,e3) => {
        if (toBoolean(eval(env,e1))) //e1 is conditional, if true evals e2, if false eval e3
          eval(env,e2)
        else
          eval(env,e3)
      }
      case Undefined => Undefined

      case _ => throw new UnsupportedOperationException


    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }

}
