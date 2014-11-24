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
    def value: List[Object]
  }

  sealed class r_self(v: DataTypeRoot = new EmptyWord) extends r {
    override def value: List[Object] = {
      v match {
        case t: t => List(t)
        case e: EmptyWord => List(e)
      }
    }
  }

  sealed class r_*(val values: List[DataTypeRoot]) extends r {
    override def value: List[Object] = {
      List() ++ values.map(value =>
        value match {
          case t: t => t
          case e: EmptyWord => e
        })
    }
  }

  sealed class r_+(val first: DataTypeRoot, val values: List[DataTypeRoot]) extends r {
    override def value: List[Object] = {
      List(first) ++ values.map(value =>
        value match {
          case t: t => t
          case e: EmptyWord => e
        })
    }
  }

  sealed class rr(val left: r, val right: r) extends r {
    override def value: List[Object] = {
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

/*http://www.openimaj.org/tutorial/sift-and-feature-matching.html*/
object LireToOpenIMAJTranformer {
  //Part one
  type LireSiftFeature = net.semanticmetadata.lire.imageanalysis.sift.Feature

  def fromLireSiftFeature(lsf: LireSiftFeature): M[Map[String, t]] = {
    val scale = new TDouble(lsf.scale); val orientation = new TDouble(lsf.orientation)
    val location = new SequenceType(lsf.location); val descriptor = new SequenceType(lsf.descriptor)

    var map: Map[String, t] = Map("scale" -> scale, "orientation" -> orientation, "location" -> location, "descriptor" -> descriptor)
    pure(map)
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

  def toOpenIMAJKeyPoint(m: M[Map[String, t]]): M[OpenIMAJKeyPoint] = {
    var scale: Float = -1; var orientation: Float = Float.MaxValue
    var location: Array[Float] = Array[Float](); var descriptor: Array[Double] = Array[Double]()
    bind(m, {
      (map: Map[String, t]) =>
        {
          map.foreach(elem =>
            if ("scale" == elem._1) {
              elem._2 match {
                case d: TDouble => scale = d.value.floatValue
                case _ => raiseSpecific("scale", "Float")
              }
            } else if ("orientation" == elem._1) {
              elem._2 match {
                case d: TDouble => orientation = d.value.floatValue
                case _ => raiseSpecific("orientation", "Float")
              }
            } else if ("location" == elem._1) {
              elem._2 match {
                case s: SequenceType => s.value match {
                  case r: r_+ => {
                    val temp = r.value.map(elem =>
                      elem match {
                        case d: TDouble => BigDecimal(d.value).setScale(1, BigDecimal.RoundingMode.HALF_UP).toFloat
                        case _ => raiseSpecific("location", "Float")
                      })
                    location = temp.map(elem => elem.asInstanceOf[Float]).toArray
                  }
                  case _ => raiseSpecific("location", "r_+")
                }
                case _ => raiseGeneral("location")
              }
            } else if ("descriptor" == elem._1) {
              val temp = elem._2 match {
                case s: SequenceType => s.value match {
                  case r: r_* => {
                    val temp = r.value.map(elem =>
                      elem match {
                        case d: TDouble => d.value
                        case _ => raiseSpecific("descriptor", "Double")
                      })
                    descriptor = temp.map(elem => elem.asInstanceOf[Double]).toArray
                  }
                  case _ => raiseSpecific("descriptor", "r_*")
                }
                case _ => raiseGeneral("descriptor")
              }
            } else raise)
        }
        assert(scale != -1)
        assert(orientation != Float.MaxValue)
        assert(location.length == 2)
        pure(new Keypoint(location(0), location(1), orientation, scale, doubleToByteArray(descriptor)))
    })

  }

  private def doubleToByteArray(in: Array[Double]): Array[Byte] = {
    //A fairly ad-hoc way to amplify the value of original and then convert it to byte 
    //and the # of keypoints generated are much less than that of OpenIMAJ Sift feature extractor
    in.map(elem => (elem * 256).toByte)
  }
}
