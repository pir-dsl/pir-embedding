package edu.uwm.cs.pir

/**
 * The below is based on the paper by Luca Cardelli at http://lucacardelli.name/Papers/BasicTypechecking.pdf
 */

trait Exp

trait DomainType extends Exp
sealed case class Document(name: String) extends DomainType
sealed case class Image(name: String) extends Exp
sealed case class Text(name: String) extends Exp

trait FeatureType extends Exp
sealed case class CEDD(name: String) extends FeatureType
sealed case class FCTH(name: String) extends FeatureType
sealed case class SIFT(name: String) extends FeatureType

trait Compose[T] extends Exp
sealed case class Histogram[T](name: String) extends Compose[T]
sealed case class Distribution[T](name: String) extends Compose[T]

trait Index[T] extends Exp
sealed case class Lucene[T](name: String) extends Index[T]
sealed case class KDTree[T](name: String) extends Index[T]


trait Model[T] extends Exp
sealed case class Cluster[T](name: String) extends Model[T]
sealed case class LDA[T](name: String) extends Model[T]

sealed case class Pair(e1: Exp, e2: Exp) extends Exp

sealed case class Id(name: String) extends Exp
sealed case class Fun(e: Exp, e_1: Exp) extends Exp
sealed case class Lambda(x: String, e: Exp) extends Exp
sealed case class Let(x: String, e: Exp, e_1: Exp) extends Exp
sealed case class Letrec(x: String, e: Exp, e_1: Exp) extends Exp

object Exp {
  def toString(ast: Exp): String = {
    var isVar = false
    val str = ast match {
      case doc: Document => { isVar = true; doc.name }
      case img: Image => { isVar = true; img.name }
      case txt: Text => { isVar = true; txt.name }
      
      case cedd: CEDD => { isVar = true; cedd.name }
      case fcth: FCTH => { isVar = true; fcth.name }
      case sift: SIFT => { isVar = true; sift.name }
      
      case hist: Histogram[t] => { isVar = true; hist.name }
      case dist: Distribution[t] => { isVar = true; dist.name }
      
      case luc: Lucene[t] => { isVar = true; luc.name }
      case kdt: KDTree[t] => { isVar = true; kdt.name }
 
      case clu: Cluster[t] => { isVar = true; clu.name }
      case lda: LDA[t] => { isVar = true; lda.name }
      
      case pair : Pair => {"(" + toString(pair.e1)  + ", " + toString(pair.e2) + ")"}
      
      case id: Id => { isVar = true; id.name }
      case l: Lambda => "fn " + l.x + " => " + toString(l.e)
      case f: Fun => toString(f.e) + " " + toString(f.e_1)
      case l: Let => "let " + l.x + " = " + toString(l.e) + " in " + toString(l.e_1)
      case lr: Letrec => "letrec " + lr.x + " = " + toString(lr.e) + " in " + toString(lr.e_1)
    }
    if (isVar) str else "(" + str + ")"
  }

}

sealed class TypeError(msg: String) extends Exception(msg)
sealed class ParseError(msg: String) extends Exception(msg)

object TypeSystem {
  trait Type
  type Gamma = Map[String, Type]

  sealed case class Var(id: Int) extends Type {
    var instance: Option[Type] = None
    lazy val name = nextUniqueName
  }

  sealed case class Operator(name: String, args: Seq[Type]) extends Type

  def Function(from: Type, to: Type) = Operator("->", Array(from, to))

  val Integer = Operator("int", Seq())
  val Bool = Operator("bool", Seq())

  var nextVarName = 'a';
  def nextUniqueName = {
    val result = nextVarName
    nextVarName = (nextVarName.toInt + 1).toChar
    result.toString
  }
  var nextVarId = 0
  def newVar: Var = {
    val result = nextVarId
    nextVarId += 1
    Var(result)
  }

  def toString(t: Type): String = t match {
    case v: Var => v.instance match {
      case Some(i) => toString(i)
      case None => v.name
    }
    case Operator(name, args) => {
      if (args.length == 0)
        name
      else if (args.length == 2)
        "(" + toString(args(0)) + " " + name + " " + toString(args(1)) + ")"
      else
        args.mkString(name + " ", " ", "")
    }
  }

  def analyze(ast: Exp, gamma: Gamma): Type = analyze(ast, gamma, Set.empty)

  def analyze(ast: Exp, gamma: Gamma, nongen: Set[Var]): Type = ast match {
    case Id(name) => getType(name, gamma, nongen)

    case Fun(e, e_1) => {
      val eType = analyze(e, gamma, nongen)
      val e_1Type = analyze(e_1, gamma, nongen)
      val resultType = newVar
      unify(Function(e_1Type, resultType), eType)
      resultType
    }

    case Lambda(x, e) => {
      val argType = newVar
      val resultType = analyze(e,
        gamma + (x -> argType),
        nongen + argType)
      Function(argType, resultType)
    }

    case Let(x, e, e_1) => {
      val eType = analyze(e, gamma, nongen)
      val newGamma = gamma + (x -> eType)
      analyze(e_1, newGamma, nongen)
    }

    case Letrec(v, e, e_1) => {
      val newType = newVar
      val newGamma = gamma + (v -> newType)
      val eType = analyze(e, newGamma, nongen + newType)
      unify(newType, eType)
      analyze(e_1, newGamma, nongen)
    }
  }

  def getType(name: String, gamma: Gamma, nongen: Set[Var]): Type = {
    if (gamma.contains(name))
      fresh(gamma(name), nongen)
    else if (isIntegerLiteral(name))
      Integer
    else
      throw new ParseError("Undefined symbol " + name)
  }

  def fresh(t: Type, nongen: Set[Var]) = {
    import scala.collection.mutable
    val mappings = new mutable.HashMap[Var, Var]
    def freshrec(tp: Type): Type = {
      prune(tp) match {
        case v: Var => if (isgeneric(v, nongen)) mappings.getOrElseUpdate(v, newVar) else v
        case Operator(name, args) => Operator(name, args.map(freshrec(_)))
      }
    }
    freshrec(t)
  }

  def unify(t1: Type, t2: Type): Unit = {
    val type1 = prune(t1)
    val type2 = prune(t2)
    (type1, type2) match {
      case (a: Var, b) => if (a != b) {
        if (occursInType(a, b)) throw new TypeError("recursive unification")
        a.instance = Some(b)
      }
      case (a: Operator, b: Var) => unify(b, a)
      case (a: Operator, b: Operator) => {
        if (a.name != b.name || a.args.length != b.args.length)
          throw new TypeError("Type mismatch: " + toString(a) + "<>" + toString(b))
        for (i <- 0 until a.args.length)
          unify(a.args(i), b.args(i))
      }
    }
  }

  // Returns the currently defining instance of t. As a side effect, collapses the list of type instances.
  def prune(t: Type): Type = t match {
    case v: Var if v.instance.isDefined => {
      var inst = prune(v.instance.get)
      v.instance = Some(inst)
      inst
    }
    case _ => t
  }

  // Note: must be called with v 'pre-pruned'
  def isgeneric(v: Var, nongen: Set[Var]) = !(occursin(v, nongen))

  // Note: must be called with v 'pre-pruned'
  def occursInType(v: Var, type2: Type): Boolean = {
    prune(type2) match {
      case `v` => true
      case Operator(name, args) => occursin(v, args)
      case _ => false
    }
  }

  def occursin(t: Var, list: Iterable[Type]) = list exists (t2 => occursInType(t, t2))

  val checkDigits = "^(\\d+)$".r
  def isIntegerLiteral(name: String) = checkDigits.findFirstIn(name).isDefined

}

object HindleyMilner {

  def main(args: Array[String]) {
    Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))

    val var1 = TypeSystem.newVar
    val var2 = TypeSystem.newVar
    val pairtype = TypeSystem.Operator("x", Array(var1, var2))

    val var3 = TypeSystem.newVar

    val myenv: TypeSystem.Gamma = Map.empty ++ Array(
      "pair" -> TypeSystem.Function(var1, TypeSystem.Function(var2, pairtype)),
      "true" -> TypeSystem.Bool,
      "cond" -> TypeSystem.Function(TypeSystem.Bool, TypeSystem.Function(var3, TypeSystem.Function(var3, var3))),
      "zero" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Bool),
      "pred" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer),
      "times" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)))

    val pair = Fun(Fun(Id("pair"), Fun(Id("f"), Id("4"))), Fun(Id("f"), Id("true")))
    val examples = Array[Exp](
      // factorial
      Letrec("factorial", // letrec factorial =
        Lambda("n", // fn n =>
          Fun(
            Fun( // cond (zero n) 1
              Fun(Id("cond"), // cond (zero n)
                Fun(Id("zero"), Id("n"))),
              Id("1")),
            Fun( // times n
              Fun(Id("times"), Id("n")),
              Fun(Id("factorial"),
                Fun(Id("pred"), Id("n")))))), // in
        Fun(Id("factorial"), Id("5"))),

      // Should fail:
      // fn x => (pair(x(3) (x(true)))
      Lambda("x",
        Fun(
          Fun(Id("pair"),
            Fun(Id("x"), Id("3"))),
          Fun(Id("x"), Id("true")))),

      // pair(f(3), f(true))
      Fun(
        Fun(Id("pair"), Fun(Id("f"), Id("4"))),
        Fun(Id("f"), Id("true"))),

      // letrec f = (fn x => x) in ((pair (f 4)) (f true))
      Let("f", Lambda("x", Id("x")), pair),

      // fn f => f f (fail)
      Lambda("f", Fun(Id("f"), Id("f"))),

      // let g = fn f => 5 in g g
      Let("g",
        Lambda("f", Id("5")),
        Fun(Id("g"), Id("g"))),

      // example that demonstrates generic and non-generic variables:
      // fn g => let f = fn x => g in pair (f 3, f true)
      Lambda("g",
        Let("f",
          Lambda("x", Id("g")),
          Fun(
            Fun(Id("pair"),
              Fun(Id("f"), Id("3"))),
            Fun(Id("f"), Id("true"))))),

      // Function composition
      // fn f (fn g (fn arg (f g arg)))
      Lambda("f", Lambda("g", Lambda("arg", Fun(Id("g"), Fun(Id("f"), Id("arg")))))))
    for (eg <- examples) {
      tryexp(myenv, eg)
    }
  }

  def tryexp(gamma: TypeSystem.Gamma, ast: Exp) {
    print(Exp.toString(ast) + " : ")
    try {
      val t = TypeSystem.analyze(ast, gamma)
      print(TypeSystem.toString(t))

    } catch {
      case t: ParseError => print(t.getMessage)
      case t: TypeError => print(t.getMessage)
    }
    println
  }
}
