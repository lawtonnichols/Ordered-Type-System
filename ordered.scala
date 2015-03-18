package cs290c

// code taken from http://bitwalker.org/blog/2013/08/10/learn-by-example-scala-parser-combinators/
// and http://zeroturnaround.com/rebellabs/parsing-lambda-calculus-in-scala/

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

/* 

S ::= FunctionDef+ ["def" id "(" ArgsWithTypes ")" ":" SingleType "=" "{" Expr+ "}"]

Expr ::= DeclAssignment ["var" id ":" SingleType "=" Expr]
       | Assignment [id "=" Expr]
       | FunctionCall [id "(" ArgsWithoutTypes ")"]
       | Int
       | Boolean ["true" | "false"]
       | Print ["print" "(" Expr ")"]
       | IfElse ["if" "(" Expr ")" "{" Expr+ "}" "else" "{" Expr+ "}"]
       | Plus [Expr "+" Expr]
       | "(" Expr ")"


SingleType ::= "Int" | "Boolean" 
Type ::= SingleType | (" TypeList ")" "=>" SingleType
ArgsWithTypes ::= ArgWithType ArgsWithTypes2 | empty string
ArgsWithTypes2 ::= "," ArgWithType ArgsWithTypes2 | empty string
ArgWithType ::= id ":" Type
ArgsWithoutTypes ::= Expr ArgsWithoutTypes2 | empty string
ArgsWithoutTypes2 ::= "," Expr ArgsWithoutTypes2 | empty string
TypeList ::= SingleType TypeList2
TypeList2 ::= "," SingleType TypeList2 | empty string



def main(arg1: Int, arg2: Boolean, arg3: (Int, Int) => Int): Int = {
	var x: Int = arg3(arg2, arg1)
	print(x)
}

*/

// the top level (start state)
trait S extends Positional
case class ListOfFunctionDefs(l: List[FunctionDef]) extends S

case class FunctionDef(name: Var, argsWithTypes: List[(Var, Type)], returnType: Type, body: List[Expr]) extends Positional

trait Expr extends Positional 
// variable declaration + assignment
case class DeclAssignment(v: Var, t: Type, e: Expr) extends Expr
// assignment of an already-declared variable
case class Assignment(v: Var, e: Expr) extends Expr
case class FunctionCall(f: Var, args: List[Expr]) extends Expr
case class EInteger(i: Int) extends Expr
case class EBoolean(b: Boolean) extends Expr
case class Print(e: Expr) extends Expr
case class IsZero(e: Expr) extends Expr
// doubles an assumption
case class Doubl(v: Var) extends Expr 
case class IfElse(pred: Expr, ifcase: List[Expr], elsecase: List[Expr]) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
// use of a variable
case class Var(s: String) extends Expr

trait Type 
case object IntType extends Type
case object BooleanType extends Type
case class FunctionType(from: List[Type], to: Type) extends Type

// used for evaluation
trait Value
case class IntValue(i: Int) extends Value
case class BooleanValue(b: Boolean) extends Value
case class FunctionValue(f: FunctionDef) extends Value

object Ordered extends JavaTokenParsers with PackratParsers {
	type P[+T] = PackratParser[T]

	lazy val s: P[S] = positioned(rep1(functionDef) ^^ ListOfFunctionDefs)
	lazy val functionDef: P[FunctionDef] = positioned("def" ~ id ~ "(" ~ argsWithTypes ~ ")" ~ ":" ~ singleType ~ "=" ~ "{" ~ rep1(expr) ~ "}" ^^ { case "def" ~ n ~ "(" ~ args ~ ")" ~ ":" ~ t ~ "=" ~ "{" ~ es ~ "}" => FunctionDef(n, args, t, es) })

	lazy val expr: P[Expr] = positioned(declAssignment | ifelse | assignment | print | iszero | double | functionCall | plus | minus | parens | boolean | id | integer)
	lazy val declAssignment: P[DeclAssignment] = positioned("var" ~ id ~ ":" ~ singleType ~ "=" ~ expr ^^ { case "var" ~ v ~ ":" ~ t ~ "=" ~ e => DeclAssignment(v, t, e) })
	lazy val assignment: P[Assignment] = positioned(id ~ "=" ~ expr ^^ { case v ~ "=" ~ e => Assignment(v, e) })
	lazy val functionCall: P[FunctionCall] = positioned(id ~ "(" ~ argsWithoutTypes ~ ")" ^^ { case v ~ "(" ~ args ~ ")" => FunctionCall(v, args) })
	lazy val integer: P[EInteger] = positioned("""-?(0|[1-9]\d*)""".r ^^ { s: String => EInteger(s.toInt) })
	lazy val boolean: P[EBoolean] = positioned("true" ^^^ { EBoolean(true) } | "false" ^^^ { EBoolean(false) })
	lazy val print: P[Print] = positioned("print" ~> "(" ~> expr <~ ")" ^^ { case e => Print(e) })
	lazy val iszero: P[IsZero] = positioned("isZero" ~> "(" ~> expr <~ ")" ^^ { case e => IsZero(e) })
	lazy val double: P[Doubl] = positioned("double" ~> "(" ~> id <~ ")" ^^ { case id => Doubl(id) })
	lazy val ifelse: P[IfElse] = positioned("if" ~ "(" ~ expr ~ ")" ~ "{" ~ rep1(expr) ~ "}" ~ "else" ~ "{" ~ rep1(expr) ~ "}" ^^ {case _~_~pred~_~_~e1s~_~_~_~e2s~_ => IfElse(pred, e1s, e2s)})
	lazy val plus: P[Plus] = positioned(expr ~ "+" ~ expr ^^ {case e1~_~e2 => Plus(e1, e2)})
	lazy val minus: P[Minus] = positioned(expr ~ "-" ~ expr ^^ {case e1~_~e2 => Minus(e1, e2)})
	lazy val parens: P[Expr] = positioned("(" ~> expr <~ ")" ^^ { case e => e })

	lazy val singleType: P[Type] = "Int" ^^^ IntType | "Boolean" ^^^ BooleanType
	lazy val typee: P[Type] = singleType | "(" ~ typeList ~ ")" ~ "=>" ~ singleType ^^ {case _~from~_~_~to => FunctionType(from, to)}
	lazy val argsWithTypes: P[List[(Var, Type)]] = argWithType ~ argsWithTypes2 ^^ {case t~ts => t::ts} | "" ^^^ List()
	lazy val argsWithTypes2: P[List[(Var, Type)]] = "," ~ argWithType ~ argsWithTypes2 ^^ {case _~a~a2 => a::a2} | "" ^^^ List()
	lazy val argWithType: P[(Var, Type)] = id ~ ":" ~ typee ^^ {case v~_~t => (v,t)}
	lazy val argsWithoutTypes: P[List[Expr]] = expr ~ argsWithoutTypes2 ^^ {case e~es => e::es} | "" ^^^ List()
	lazy val argsWithoutTypes2: P[List[Expr]] = "," ~ expr ~ argsWithoutTypes2 ^^ {case _~e~es => e::es}  | "" ^^^ List()
	lazy val typeList: P[List[Type]] = singleType ~ typeList2 ^^ {case t~ts => t::ts}
	lazy val typeList2: P[List[Type]] = "," ~ singleType ~ typeList2 ^^ {case _~t~ts => t::ts} | "" ^^^ List()
	
	lazy val id: P[Var] = """[_a-zA-Z][A-Za-z0-9_]*""".r ^^ {case s => Var(s)}

	/******** BEGIN TYPE CHECKER ********/

	// for use in function calls
	def getTypesOfAllFunctions(l: ListOfFunctionDefs): Map[Var, FunctionType] = l.l.foldLeft(Map[Var, FunctionType]())((acc, x) => acc + (x.name -> FunctionType(x.argsWithTypes.map(_._2),x.returnType)))

	// type check an entire list of functions--returns true if they all type check
	def typeCheck(l: ListOfFunctionDefs): Boolean = {
		val functions = getTypesOfAllFunctions(l)
		l.l.foldLeft(true)((acc, x) => if (typeCheck(x, functions)) acc else false)
	}

	// type check a single function definition
	// only need to return a Boolean, because no environment is passed between functions
	def typeCheck(f: FunctionDef, functions: Map[Var, FunctionType]): Boolean = {
		val initialAssumptions = f.argsWithTypes
		val (ty, finalAssumptions) = typeCheckFunctionBody(f.body, initialAssumptions, functions)
		// function must match its declared return type
		if (ty != f.returnType) {
			println(f.pos + ":")
			println("return type of function " + f.name.s + " declared to be " + f.returnType + ", but typechecks to be " + ty)
			sys.exit()
		}
		// a function needs to use all its assumptions
		if (!finalAssumptions.isEmpty) {
			println(f.pos + ":")
			println("function " + f.name.s + " does not use every assumption. left over assumptions: " + finalAssumptions)
			sys.exit()
		}
		true
	}

	// type check each expression in a function body in order,
	// updating the assumptions as we go
	// return (returnType, new list of assumptions)
	def typeCheckFunctionBody(b: List[Expr], assumptions: List[(Var, Type)], functions: Map[Var, FunctionType]): (Type, List[(Var, Type)]) = {
		b match {
			case Nil => {
				println("empty function body")
				sys.exit()
			}
			case x :: Nil => {
				typeCheck(x, assumptions, functions)
			}
			case x :: xs => {
				val (ty, newAssumptions) = typeCheck(x, assumptions, functions)
				typeCheckFunctionBody(xs, newAssumptions, functions) 
			}
		}
	}

	// type check the arguments given to a function
	// we need to return all the types of those arguments
	// return (list of types of arguments, new list of assumptions)
	def typeCheckFunctionArguments(args: List[Expr], assumptions: List[(Var, Type)], functions: Map[Var, FunctionType]): (List[Type], List[(Var, Type)]) = {
		args match {
			case Nil => (List(), assumptions)
			case x :: xs => {
				val (ty, newAssumptions) = typeCheck(x, assumptions, functions)
				val (recty, finalAssumptions) = typeCheckFunctionArguments(xs, newAssumptions, functions)
				(ty :: recty, finalAssumptions)
			}
		}
	}

	// Typecheck an expression--the main part of the typechecker. 
	// Returns (type of the given expression, updated list of expressions)
	def typeCheck(e: Expr, assumptions: List[(Var, Type)], functions: Map[Var, FunctionType]): (Type, List[(Var, Type)]) = {
		e match {
			case d@DeclAssignment(v, t, e) => {
				val (ty, newAssumptions) = typeCheck(e, assumptions, functions)
				if (ty == t)
					(ty, newAssumptions ++ List((v, t))) // add the variable v of type t to the assumptions
				else {
					println(e.pos + ":")
					println(s"$d : the declared type and the type of $e do not match")
					sys.exit()
				}
			}
			case a@Assignment(v, e) => {
				if (!assumptions.isEmpty && assumptions.last._1 == v) {
					val (ety, newAssumptions) = typeCheck(e, assumptions, functions)
					if (ety != assumptions.last._2) {
						println(a.pos + ":")
						println(s"$a : the type of $v and the type of $e do not match")
						sys.exit()
					} else {
						(ety, assumptions.init ++ List((v, ety))) // add v and it's type back onto the list of assumptions
					}
				} else {
					println(a.pos + ":")
					println(s"$a : can't find variable $v")
					sys.exit()
				}
			}
			case fc@FunctionCall(f, args) => {
				val functionName = f.s
				var newAssumptions = assumptions
				var functionType: Type = FunctionType(List(), IntType) // initial dummy function type
				// first check if f is in the list of assumptions
				if (!assumptions.isEmpty && assumptions.last._1 == f) {
					// it is, so we need to use it
					newAssumptions = assumptions.init
					functionType = assumptions.last._2
					functionType match {
						case f : FunctionType => ()
						case _ => {
							println(f.pos + ":")
							println(s"the function $f needs to be of function type")
							sys.exit()
						}
					}
				} else {
					// if not we need to use one of our defined functions (which can be used an unlimited amount of times)
					if (functions.contains(f))
						functionType = functions(f)
					else {
						println(f.pos + ":")
						println(s"cannot find function $f for use in $fc")
						sys.exit()
					}
				}

				val actualFunctionType: FunctionType = functionType.asInstanceOf[FunctionType]

				if (actualFunctionType.from.length != args.length) {
					println(fc.pos + ":")
					println(s"$fc : incorrect number of arguments")
					sys.exit()
				}

				// typecheck the arguments
				val (argTypes, newNewAssumptions) = typeCheckFunctionArguments(args, newAssumptions, functions)

				// the given arguments' types better match the function's argument types
				if (argTypes == actualFunctionType.from)
					(actualFunctionType.to, newNewAssumptions) // return the type that the function returns
				else {
					println(fc.pos + ":")
					println(s"$fc : incorrect argument types. expected " + actualFunctionType.from + s", got $argTypes")
					sys.exit()
				}
			}
			case EInteger(i) => (IntType, assumptions)
			case EBoolean(b) => (BooleanType, assumptions)
			case Print(e) => typeCheck(e, assumptions, functions) // use the assumptions in e
			case ie@IfElse(pred, ifcase, elsecase) => {
				val (ty, newAssumptions) = typeCheck(pred, assumptions, functions)
				if (ty != BooleanType){
					println(pred.pos + ":")
					println(s"the predicate of an if statement must be of type Boolean: $ie")
					sys.exit()
				}
				val (ifty, ifNewAssumptions) = typeCheckFunctionBody(ifcase, newAssumptions, functions)
				val (elsety, elseNewAssumptions) = typeCheckFunctionBody(elsecase, newAssumptions, functions)
				if (ifty != elsety) {
					println(ie.pos + ":")
					println(s"both the if and else cases must have the same type: $ie")
					sys.exit()
				}
				if (ifNewAssumptions != elseNewAssumptions) {
					println(ie.pos + ":")
					println(s"both the if and else cases must use the same assumptions: $ie")
					sys.exit()
				}
				(ifty, ifNewAssumptions) // they're both the same; could use elsety and elseNewAssumptions here
			}
			case Plus(e1, e2) => {
				val (ty, newAssumptions) = typeCheck(e1, assumptions, functions)
				val (ty2, newNewAssumptions) = typeCheck(e2, newAssumptions, functions)
				if (ty != IntType) {
					println(e1.pos + ":")
					println(s"$e1 needs to be an Int")
					sys.exit()
				}
				if (ty2 != IntType) {
					println(e2.pos + ":")
					println(s"$e2 needs to be an Int")
					sys.exit()
				}
				(IntType, newNewAssumptions)
			}
			case Minus(e1, e2) => {
				val (ty, newAssumptions) = typeCheck(e1, assumptions, functions)
				val (ty2, newNewAssumptions) = typeCheck(e2, newAssumptions, functions)
				if (ty != IntType) {
					println(e1.pos + ":")
					println(s"$e1 needs to be an Int")
					sys.exit()
				}
				if (ty2 != IntType) {
					println(e2.pos + ":")
					println(s"$e2 needs to be an Int")
					sys.exit()
				}
				(IntType, newNewAssumptions)
			}
			case v@Var(s) => {
				if (assumptions.isEmpty && !functions.contains(v)) {
					println(e.pos + ":")
					println(s"can't typecheck a variable with an empty list of assumptions: $s")
					sys.exit()
				}
				if (!assumptions.isEmpty && assumptions.last._1 != v && !functions.contains(v)) {
					println(e.pos + ":")
					println(s"can't use variable $s right now; check your assumption use order")
					sys.exit()
				}
				// otherwise it's there; return the type and pop the assumption
				if (!assumptions.isEmpty && assumptions.last._1 == v)
					(assumptions.last._2, assumptions.init)
				else {
					(functions(v), assumptions) // if we got this far functions better contain v
				}
			}
			case IsZero(e) => {
				val (ty, newAssumptions) = typeCheck(e, assumptions, functions)
				if (ty == IntType) {
					(BooleanType, newAssumptions)
				} else {
					println(e.pos + ":")
					println(s"$e must be of type int")
					sys.exit()
				}
			}
			case Doubl(v) => {
				val (ty, newAssumptions) = typeCheck(v, assumptions, functions)
				(ty, newAssumptions ++ List((v,ty),(v,ty)))
			}
			case x => {
				println(e.pos + ":")
				println(s"$x : can't type check this for some reason")
				sys.exit()
			}
		}
	}

	/******** END TYPE CHECKER ********/

	/******** BEGIN  EVALUATOR ********/

	def evaluate(l: ListOfFunctionDefs): Unit = {
		// create a function map, taking function names to function definitions
		val functionMap = l.l.foldLeft(Map[Var, FunctionDef]())((acc, x) => acc + (x.name -> x))
		if (!functionMap.contains(Var("main"))) {
			println("need a main function")
			sys.exit()
		}
		evaluate(functionMap(Var("main")), List(), functionMap)
	}

	def evaluate(f: FunctionDef, args: List[(Var,Value)], functionMap: Map[Var, FunctionDef]): Value = {
		evaluateListOfExpressions(f.body, Map() ++ args, functionMap)
	}

	def evaluateListOfExpressions(l: List[Expr], variableMap: Map[Var,Value], functionMap: Map[Var, FunctionDef]): Value = {
		l match {
			case Nil => IntValue(-1) // dummy value; should never get here if we typechecked
			case x :: Nil => {
				val (v, newMap) = evaluate(x, variableMap, functionMap)
				v
			}
			case x :: xs => {
				val (v, newMap) = evaluate(x, variableMap, functionMap)
				evaluateListOfExpressions(xs, newMap, functionMap)
			}
		}
	}

	// this one returns the variable context too
	def evaluateListOfExpressions2(l: List[Expr], variableMap: Map[Var,Value], functionMap: Map[Var, FunctionDef]): (Value, Map[Var, Value]) = {
		l match {
			case Nil => (IntValue(-1), variableMap) // dummy value
			case x :: Nil => {
				val (v, newMap) = evaluate(x, variableMap, functionMap)
				(v, newMap)
			}
			case x :: xs => {
				val (v, newMap) = evaluate(x, variableMap, functionMap)
				evaluateListOfExpressions2(xs, newMap, functionMap)
			}
		}
	}

	def evaluateFunctionArgs(l: List[Expr], variableMap: Map[Var,Value], functionMap: Map[Var, FunctionDef]): (List[Value], Map[Var,Value]) = {
		l match {
			case Nil => (List(), variableMap)
			case x :: xs => {
				val (v, newMap) = evaluate(x, variableMap, functionMap)
				val (rest, newNewMap) = evaluateFunctionArgs(xs, newMap, functionMap)
				(v::rest, newNewMap)
			}
		}
	}

	// evaluate an individual expression
	def evaluate(e: Expr, variableMap: Map[Var,Value], functionMap: Map[Var, FunctionDef]): (Value, Map[Var,Value]) = {
		e match {
			case DeclAssignment(v, t, e) => {
				val (va, newMap) = evaluate(e, variableMap, functionMap)
				(va, newMap + (v -> va))
			}
			case Assignment(v, e) => {
				val (va, newMap) = evaluate(e, variableMap, functionMap)
				(va, newMap + (v -> va))
			}
			case FunctionCall(f, args) => {
				val func = {
					if (variableMap.contains(f))
						variableMap(f).asInstanceOf[FunctionValue].f
					else
						functionMap(f)
				}
				val (argsEvaluated, newMap) = evaluateFunctionArgs(args, variableMap, functionMap)
				val v = evaluate(func, func.argsWithTypes.map(_._1).zip(argsEvaluated), functionMap)

				(v, newMap)
			}
			case EInteger(i) => (IntValue(i), variableMap)
			case EBoolean(b) => (BooleanValue(b), variableMap)
			case Print(e) => {
				val (v, newMap) = evaluate(e, variableMap, functionMap)
				v match {
					case IntValue(i) => println(i)
					case BooleanValue(b) => println(b)
				}
				(v, newMap)
			}
			case IsZero(e) => {
				val (v, newMap) = evaluate(e, variableMap, functionMap)
				val res = {
					if (v.asInstanceOf[IntValue].i == 0)
						BooleanValue(true)
					else
						BooleanValue(false)
				}
				(res, newMap)
			}
			case Doubl(v) => {
				(IntValue(-1), variableMap) // dummy value, just because this has to evaluate to something
			}
			case IfElse(pred, ifcase, elsecase) => {
				val (b, newMap) = evaluate(pred, variableMap, functionMap)
				if (b.asInstanceOf[BooleanValue].b == true) {
					evaluateListOfExpressions2(ifcase, newMap, functionMap)
				} else {
					evaluateListOfExpressions2(elsecase, newMap, functionMap)
				}
			}
			case Plus(e1, e2) => {
				val (v1, newMap) = evaluate(e1, variableMap, functionMap)
				val (v2, newNewMap) = evaluate(e2, newMap, functionMap)
				(IntValue(v1.asInstanceOf[IntValue].i + v2.asInstanceOf[IntValue].i), newNewMap)
			}
			case Minus(e1, e2) => {
				val (v1, newMap) = evaluate(e1, variableMap, functionMap)
				val (v2, newNewMap) = evaluate(e2, newMap, functionMap)
				(IntValue(v1.asInstanceOf[IntValue].i - v2.asInstanceOf[IntValue].i), newNewMap)
			}
			case v@Var(s) => {
				if (variableMap.contains(v))
					(variableMap(v), variableMap)
				else
					(FunctionValue(functionMap(v)), variableMap)
			}
		}
	}

	/******** END EVALUATOR ********/

	def main(args: Array[String]): Unit = {
		if (args.length != 1) {
			println("usage: scala Ordered [program file]")
			sys.exit()
		}

		val input = scala.io.Source.fromFile(args(0)).mkString
		val parse = parseAll(s, input.replaceAll("//.*", ""))
		println(parse)
		println()
		if (parse.successful) {
			val parsedExpression = parse.get
			if (typeCheck(parsedExpression.asInstanceOf[ListOfFunctionDefs])) {
				println("Passed the typechecker!")
				println()
				evaluate(parsedExpression.asInstanceOf[ListOfFunctionDefs])
			}
		}
	}
}
