package edu.uwm.cs.pir

import org.openimaj.feature.local.list.LocalFeatureList
import org.openimaj.image.feature.local.keypoints.Keypoint

import collection.JavaConversions._
import edu.uwm.cs.pir.DataType._

object DataType {

  trait DataTypeRoot

  // Basic types that can be used to represent types from heterogeneous systems
  trait t extends DataTypeRoot
  sealed class SingletonType[T <: t](x: T) extends t
  sealed class ClosedRecord[T <: t](val seq: List[T] = List[T]()) extends t
  sealed class OpenRecord[T <: t](val value: T = null, var tail: OpenRecord[T]) extends t {
    def cons(newRecord: OpenRecord[T]) = {
      if (newRecord.value != null) {
        var current = tail
        while (current.value != null) {
          current = current.tail
        }
        current = newRecord
      }
    }
  }
  sealed class SequenceType(val value: r) extends t
  sealed class SpecialType(value: SpecialRoot) extends t
  sealed class UnionType[X1 <: t, X2 <: t](val x1: X1, x2: X2) extends t

  trait SpecialRoot extends t
  sealed class TNull(val value: Null) extends SpecialRoot
  sealed class TNothing(val value: Nothing) extends SpecialRoot
  sealed class TAny(val value: Any) extends SpecialRoot

  trait BaseRoot extends t
  sealed class TChar(val value: Char) extends BaseRoot
  sealed class TInt(val value: Int) extends BaseRoot
  sealed class TDouble(val value: Double) extends BaseRoot

  sealed class EmptyWord extends DataTypeRoot {
    val value = ""
  }

  trait r {
    def value: List[DataTypeRoot]
  }

  sealed class r_self(v: DataTypeRoot = new EmptyWord) extends r {
    override def value: List[DataTypeRoot] = {
      v match {
        case t: t => List(t)
        case e: EmptyWord => List(e)
      }
    }
  }

  sealed class r_*(val values: List[DataTypeRoot]) extends r {
    override def value: List[DataTypeRoot] = {
      List() ++ values.map(value =>
        value match {
          case t: t => t
          case e: EmptyWord => e
        })
    }
  }

  sealed class r_+(val first: DataTypeRoot, val values: List[DataTypeRoot]) extends r {
    override def value: List[DataTypeRoot] = {
      List(first) ++ values.map(value =>
        value match {
          case t: t => t
          case e: EmptyWord => e
        })
    }
  }

  sealed class rr(val left: r, val right: r) extends r {
    override def value: List[DataTypeRoot] = {
      left.value ++ right.value
    }
  }

  def getName[T](t: T): String = {
    t.getClass.getSimpleName
  }

  /*--------------Monad definition Begin--------------*/
  type Exception = String
  trait M[A] {
    def getA: A
  }
  sealed case class Raise[A](e: Exception) extends M[A] {
    override def getA: A = ???
  }
  sealed case class Return[A](val a: A) extends M[A] {
    override def getA: A = a
  }

  def pure[A](a: A): M[A] = Return(a)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] = {
    m match {
      case Raise(e) => Raise(e)
      case Return(a) => k(a)
    }
  }
  def raise[A]: M[A] = throw new RuntimeException("unrecoganized varible")
  def raiseSpecific[A](varName: String, correctType: String): M[A] = throw new RuntimeException(varName + " needs to be of " + correctType + " type")
  def raiseGeneral[A](varName: String): M[A] = throw new RuntimeException("The incoming type of " + varName + " has to of PIR internal DataType t")
  /*--------------Monad definition End--------------*/
}

import scala.reflect.runtime.universe._
import scala.collection.mutable.Queue
sealed class PirFeature(val map: Map[String, t], val types: Queue[Type])

/*http://www.openimaj.org/tutorial/sift-and-feature-matching.html*/
object LireToOpenIMAJTranformer {
  //Part one
  type LireSiftFeature = net.semanticmetadata.lire.imageanalysis.sift.Feature

  def fromLireSiftFeature(lsf: LireSiftFeature): M[PirFeature] = {
    val scale = new TDouble(lsf.scale); val orientation = new TDouble(lsf.orientation)
    val location_x = new TDouble(lsf.location(0)); val location_y = new TDouble(lsf.location(1));
    val descriptor = new SequenceType(lsf.descriptor)

    val map: Map[String, t] = Map("scale" -> scale, "orientation" -> orientation, "location_x" -> location_x, "location_y" -> location_y, "descriptor" -> descriptor)
    pure(new PirFeature(map, Queue(typeOf[LireSiftFeature])))
  }

  implicit def double_to_r_*(doubleArr: Array[Double]): r = {
    val lst = doubleArr.toList.map(i => new TDouble(i))
    new r_*(lst)
  }

  implicit def float_to_r_+(floatArr: Array[Float]): r = {
    val lst = floatArr.toList.map(i => new TDouble(i: Double))
    new r_+(lst.head, lst.tail)
  }

  //Part 2
  type OpenIMAJKeyPoint = Keypoint

  //val engine = new org.openimaj.image.feature.local.engine.DoGSIFTEngine;	
  //val queryKeypoints : LocalFeatureList[Keypoint] = engine.findFeatures(query.flatten());

  def toOpenIMAJKeyPoint(m: M[PirFeature]): M[OpenIMAJKeyPoint] = {
    var scale: Float = -1; var orientation: Float = Float.MaxValue
    var location: Array[Float] = new Array[Float](2); var descriptor: Array[Double] = Array[Double]()
    bind(m, {
      (pirFeature: PirFeature) =>
        {
          typeCheck(pirFeature, getRules("OpenIMAJ", typeTag[Keypoint]))
          pirFeature.map.foreach(elem =>
            {
              elem._2 match {
                case d: TDouble if ("scale" == elem._1) => scale = d.value.floatValue
                case d: TDouble if ("orientation" == elem._1) => orientation = d.value.floatValue
                case d: TDouble if ("location_x" == elem._1) => location(0) = d.value.floatValue
                case d: TDouble if ("location_y" == elem._1) => {
                  val x = d.value.floatValue
                  location(1) = d.value.floatValue
                }
                case s: SequenceType => s.value match {
                  //                  case r: r_+ => {
                  //                    val temp = r.value.map(elem =>
                  //                      elem match {
                  //                        case d: TDouble => BigDecimal(d.value).setScale(1, BigDecimal.RoundingMode.HALF_UP).toFloat
                  //                        case _ => raiseSpecific("location", "Float")
                  //                      })
                  //                    location = temp.map(elem => elem.asInstanceOf[Float]).toArray
                  //                  }
                  case r: r_* => {
                    val temp = r.value.map(elem =>
                      elem match {
                        case d: TDouble => d.value
                        case _ => raiseSpecific("descriptor", "Double")
                      })
                    descriptor = temp.map(elem => elem.asInstanceOf[Double]).toArray
                  }
                  case _ if ("location" == elem._1) => raiseSpecific(elem._1, "r_+")
                  case _ if ("descriptor" == elem._1) => raiseSpecific(elem._1, "r_*")
                  case _ => raise
                }
                case _ if (("scale" == elem._1) || ("orientation" == elem._1)) => raiseSpecific(elem._1, "Float")
                case _ => raise
              }
            })
          assert(scale != -1)
          assert(orientation != Float.MaxValue)
          pure(new Keypoint(location(0), location(1), orientation, scale, doubleToByteArray(descriptor)))
        }
    })
  }

  private def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

  private def getRules[T](system: String, tpe: TypeTag[T]): Map[String, t => Boolean] = {
    var map: Map[String, t => Boolean] = Map()
    if ("OpenIMAJ" == system) {
      val decls = tpe.tpe.declarations
      decls.map(decl => {
        if (decl.isTerm) {
          val term = decl.asTerm
          if (term.isVar) {
            //println("var : " + term.name + ", type: " + term.typeSignature)
            term.typeSignature match {
              case t if t =:= typeOf[Array[Byte]] => map = map + (term.name.toString -> ((s: t) => if (s.isInstanceOf[SequenceType]) true else false))
              case t if t =:= typeOf[Float] => map = map + (term.name.toString -> ((s: t) => if (s.isInstanceOf[TDouble]) true else false))
              case t if t =:= typeOf[Double] => map = map + (term.name.toString -> ((s: t) => if (s.isInstanceOf[TDouble]) true else false))
              case t if t =:= typeOf[Array[Float]] => map = map + (term.name.toString -> ((s: t) => if (s.isInstanceOf[SequenceType]) true else false))
              case t if t =:= typeOf[Array[Double]] => map = map + (term.name.toString -> ((s: t) => if (s.isInstanceOf[SequenceType]) true else false))
              case _ => throw new RuntimeException("Cannot process " + term.typeSignature + " type for variable " + term.name + " in " + decl + ".")
            }
          }
        }
      })
    }
    map
  }

  private val lireSiftToOpenIMAJKeyPointVarNameMap = Map("scale" -> "scale", "orientation" -> "ori", "location_x" -> "x", "location_y" -> "y", "descriptor" -> "ivec")

  private def typeCheck(pirFeature: PirFeature, rules: Map[String, t => Boolean]): Unit = {

    //If more rules exist than # of variables, fail
    if (rules.size > pirFeature.map.size)
      throw new RuntimeException("No enough variables have been included for typing rules, please check!")
    pirFeature.map.map(elem => {
      // No rule exists for a variable, we skip
      if (rules(lireSiftToOpenIMAJKeyPointVarNameMap(elem._1)) != null) {
        // Rule not check, fail  
        if (!rules(lireSiftToOpenIMAJKeyPointVarNameMap(elem._1))(elem._2))
          throw new RuntimeException(elem._2.getClass.getName + " does not satisify the type requirement " + rules(lireSiftToOpenIMAJKeyPointVarNameMap(elem._1)))
      }
    })
  }

  private def doubleToByteArray(in: Array[Double]): Array[Byte] = {
    //A fairly ad-hoc way to amplify the value of original and then convert it to byte 
    //and the # of keypoints generated are much less than that of OpenIMAJ Sift feature extractor
    in.map(elem => (elem * 256).toByte)
  }
}
