/**
 * Parser wyrażeń arytmetycznych
 * Swoją drogą Scala to bardzo ciekawy język...
 * 
 * @Author: Peter Schmitz
 * @Source: https://stackoverflow.com/questions/5805496/arithmetic-expression-grammar-and-parser
 */
package backend;

import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random

class FormulaParser(val constants: Map[String,Double] = Map(), val userFcts: Map[String,String => Double] = Map(), random: Random = new Random) extends JavaTokenParsers {  
  require(constants.keySet.intersect(userFcts.keySet).isEmpty)
  
  private def log(a : Double, b : Double) : Double = math.log(b) / math.log(a)
  // Nie jestem w stanie tak zmodyfikować gramatyki, aby dopuszczała same nawiasy...
  // To: private def bracket: Parser[Double] = "("~>expression<~")" ^^ { case e1 => parenthesis(e1) } powinno łapać, a jednak nic nie wychodzi...
  private def parenthesis(a : Double) : Double = { println(a); return (a) }
  private val allConstants = constants ++ Map("E" -> E, "PI" -> Pi, "Pi" -> Pi, "fi" -> (1.0+sqrt(5))/2, "g" -> 0.5772156649, "k" -> 2.6854520)
  private val unaryOps: Map[String,Double => Double] = Map(
   "sqrt" -> (sqrt(_)), "abs" -> (abs(_)), "floor" -> (floor(_)), "ceil" -> (ceil(_)), "ln" -> (math.log(_)), "round" -> (round(_)), "signum" -> (signum(_)),
   "exp" -> (exp(_)), "log" -> (log10(_)), "_" -> (parenthesis(_)),
   "acos" -> (acos(_)), "asin" -> (asin(_)), "atan" -> (atan(_)),
   "cos" -> (cos(_)), "sin" -> (sin(_)), "tan" -> (tan(_)), "ctg" -> (1.0/tan(_)),
   "cosh" -> (cosh(_)), "sinh" -> (sinh(_)), "tanh" -> (tanh(_))
  )
  private val binaryOps1: Map[String,(Double,Double) => Double] = Map(
   "+" -> (_+_), "-" -> (_-_), "*" -> (_*_), "/" -> (_/_), "^" -> (pow(_,_))
  )
  private val binaryOps2: Map[String,(Double,Double) => Double] = Map(
   "max" -> (max(_,_)), "min" -> (min(_,_)), "log" -> (log(_,_))
  )
  private def fix(t : String) = if(t.length == 0) "0" else t
  private def fold(d: Double, l: List[~[String,Double]]) = l.foldLeft(d){ case (d1,op~d2) => binaryOps1(op)(d1,d2) } 
  private implicit def map2Parser[V](m: Map[String,V]) = m.keys.map(_ ^^ (identity)).reduceLeft(_ | _)
  private def expression:  Parser[Double] = sign~term~rep(("+"|"-")~term) ^^ { case s~t~l => fold(s * t,l) }
  private def sign:        Parser[Double] = opt("+" | "-") ^^ { case None => 1; case Some("+") => 1; case Some("-") => -1 }
  private def term:        Parser[Double] = longFactor~rep(("*"|"/")~longFactor) ^^ { case d~l => fold(d,l) }
  private def longFactor:  Parser[Double] = shortFactor~rep("^"~shortFactor) ^^ { case d~l => fold(d,l) }
  private def shortFactor: Parser[Double] = fpn | sign~(constant | rnd | unaryFct | binaryFct | userFct | "("~>expression<~")") ^^ { case s~x => s * x }
  private def constant:    Parser[Double] = allConstants ^^ (allConstants(_))
  private def rnd:         Parser[Double] = "rnd"~>"("~>fpn~","~fpn<~")" ^^ { case x~_~y => require(y > x); x + (y-x) * random.nextDouble } | "rnd" ^^ { _ => random.nextDouble }
  private def fpn:         Parser[Double] = floatingPointNumber ^^ (_.toDouble) 
  private def unaryFct:    Parser[Double] = unaryOps~"("~expression~")" ^^ { case op~_~d~_ => unaryOps(op)(d) }
  private def binaryFct:   Parser[Double] = binaryOps2~"("~expression~","~expression~")" ^^ { case op~_~d1~_~d2~_ => binaryOps2(op)(d1,d2) }
  private def userFct:     Parser[Double] = userFcts~"("~(expression ^^ (_.toString) | ident)<~")" ^^ { case fct~_~x => userFcts(fct)(x) }
  def evaluate(formula: String) = parseAll(expression, fix(formula)).get
}