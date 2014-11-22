package edu.uwm.cs.pir

object DataType {

  trait DataTypeRoot
  
  // Basic types that can be used to represent types from heterogeneous systems
  trait t extends DataTypeRoot
  sealed class SingletonType[T <: t](x: T) extends t {}
  sealed class ClosedRecord[T <: t](val seq: List[T] = List[T]()) extends t {}
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
  sealed class SequenceType(val value: r) extends t {}
  sealed class BaseType(val value: BaseRoot) extends t {}
  sealed class SpecialType(value: SpecialRoot) extends t {}
  sealed class UnionType[X1 <: t, X2 <: t](val x1: X1, x2: X2) extends t {}

  trait SpecialRoot
  sealed class TNull (val value : Null) extends SpecialRoot
  sealed class TNothing (val value : Nothing) extends SpecialRoot
  sealed class TAny (val value : Any) extends SpecialRoot
  
  trait BaseRoot
  sealed class TChar (val value : Char) extends BaseRoot
  sealed class TInt (val value : Int) extends BaseRoot
  sealed class TDouble (val value : Double) extends BaseRoot
  
  sealed class EmptyWord extends DataTypeRoot {
    val value = ""
  }

  trait r {
    def value: List[Object]
  }

  sealed class r_self(v: DataTypeRoot = new EmptyWord) extends r {
    override def value: List[Object] = {
      v match {
        case t : t => List(t)
        case e : EmptyWord => List(e)
      }
    }
  }

  sealed class r_*(val values: List[DataTypeRoot]) extends r {
    override def value: List[Object] = {
      List() ++ values.map(value =>
        value match {
          case t : t => t
          case e : EmptyWord => e
        })
    }
  }

  sealed class r_+(val first: r, val values: List[DataTypeRoot]) extends r {
    override def value: List[Object] = {
      first.value ++ values.map(value =>
        value match {
          case t : t => t
          case e : EmptyWord => e
        })
    }
  }

  sealed class rr(val left: r, val right: r) extends r {
    override def value: List[Object] = {
      left.value ++ right.value
    }
  }

  /**
   * We may not need the below two; i.e. optional and check type
   */
  //  sealed class r_or_r(val v1: r, val v2: r) extends r {
  //    override def value: List[Object] = {
  //      //TODO
  //    }
  //  }
  //  
  //  sealed class r_?(val undefined: Nothing, val value: r) extends r {
  //    override def value: List[Object] = {
  //      //TODO
  //    }
  //  }

/** Not used yet, but like to keep it here
 */  
/*  type ^[T] = T => Nothing
  type v[T1, T2] = ^[^[T1] with ^[T2]]
  type ^^[T] = ^[^[T]]
  type or[X, T1, T2] = ^^[X] <:< (T1 v T2)
  type |[T1, T2] = { type or[X] = ^^[X] <:< (T1 v T2) }*/

  //  trait Disj[T] {
  //    type or[S] = Disj[T with ^[S]]
  //    type apply = ^[T]
  //  }
  //  type disj[T] = { type or[S] = Disj[^[T]]#or[S] }
  //
  //  ////////////////////////////////////////////////////

  //  def let[A, B](x: A)(f: A => B): B = f(x)
  ////////////////////////////////////////////////////
  def getName[T](t: T): String = {
    t.getClass.getSimpleName
  }
}

//http://www.openimaj.org/tutorial/sift-and-feature-matching.html

import org.openimaj.feature.local.list.LocalFeatureList
import org.openimaj.image.feature.local.keypoints.Keypoint

import collection.JavaConversions._
import edu.uwm.cs.pir.DataType._
object LireToMapTranformer {
  type LireSiftFeature = net.semanticmetadata.lire.imageanalysis.sift.Feature

  def transformFromLireSiftFeature(lsf: LireSiftFeature): Map[String, t] = {
    val scale: BaseType = new BaseType(new TDouble(lsf.scale))
    val orientation: BaseType = new BaseType(new TDouble(lsf.orientation))
    val location: SequenceType = new SequenceType(lsf.location)
    val descriptor: SequenceType = new SequenceType(lsf.descriptor)

    var map: Map[String, t] = Map(
      "scale" -> scale,
      "orientation" -> orientation,
      "location" -> location,
      "descriptor" -> descriptor)
    map
  }

  implicit def double_to_r_*(doubleArr: Array[Double]): r = {
    val lst = doubleArr.toList.map(i => new BaseType(new TDouble(i)))
    new r_*(lst)
  }

  implicit def float_to_r_*(floatArr: Array[Float]): r = {
    val lst = floatArr.toList.map(i => new BaseType(new TDouble(i: Double)))
    new r_*(lst)
  }
}

object MapToOpenIMAJTranformer {
  type OpenIMAJKeyPoint = Keypoint
 
  //val engine = new org.openimaj.image.feature.local.engine.DoGSIFTEngine;	
  //val queryKeypoints : LocalFeatureList[Keypoint] = engine.findFeatures(query.flatten());

  def transformToOpenIMAJKeyPoint(map: Map[String, t]): OpenIMAJKeyPoint = {
    val scale: Float = map("scale") match {
      case b: BaseType => b.value match {
        case char : TChar => handleInvalidTypeException("scale", "Float")
        case i : TInt => handleInvalidTypeException("scale", "Float")
        case d : TDouble => d.value.floatValue
      }
      case _ => handleGeneralTypeException("scale")
    }

    val orientation: Float = map("orientation") match {
      case b: BaseType => b.value match {
        case char : TChar => handleInvalidTypeException("orientation", "Float")
        case i : TInt => handleInvalidTypeException("orientation", "Float")
        case d : TDouble => d.value.floatValue
      }
      case _ => handleGeneralTypeException("orientation")
    }

    val location: Array[Float] = map("location") match {
      case s: SequenceType => s.value match {
        case r: r_* => r.value.map(elem =>
          elem match {
            case b: BaseType => b.value match {
              case d : TDouble => BigDecimal(d.value).setScale(1, BigDecimal.RoundingMode.HALF_UP).toFloat
              case _ => handleGeneralTypeException("location")
            }
            case _ => handleGeneralTypeException("location")
          }).toArray
        case _ => handleInvalidTypeException("location", "r_*")
      }
      case _ => handleGeneralTypeException("location")
    }

    assert(location.length == 2)

    val descriptor: Array[Double] = map("descriptor") match {
      case s: SequenceType => s.value match {
        case r: r_* => r.value.map(elem =>
          elem match {
            case b: BaseType => b.value match {
              case d : TDouble => d.value
              case _ => handleGeneralTypeException("location")
            }
            case _ => handleGeneralTypeException("location")
          }).toArray
        case _ => handleInvalidTypeException("descriptor", "r_*")
      }
      case _ => handleGeneralTypeException("location")
    }

    val descriptorInBytes: Array[Byte] = doubleToByteArray(descriptor)
    new Keypoint(location(0), location(1), orientation, scale, descriptorInBytes)
  }

  private def doubleToByteArray(in: Array[Double]): Array[Byte] = {
    //A fairly ad-hoc way to amplify the value of original and then convert it to byte
    //The precision is affected and thus the output keypoints is much less than directly output
    //from OpenIMAJ Sift feature extractor
    in.map(elem => (elem * 256).toByte)
  }
//  import net.semanticmetadata.lire.utils.SerializationUtils
//  private def doubleToByteArray(in: Array[Double]): Array[Byte] = {
//    var result = new Array[Byte](in.size * 4)
//    var i = 0
//    for (i <- 0 until result.length by 4) {
//      val tmp = SerializationUtils.toBytes(in(i / 4))
//      var j = 0
//      for (j <- 0 until 4 by 1) {
//        result(i + j) = tmp(j)
//      }
//    }
//    result
//  }

  private def handleInvalidTypeException(varName: String, correctType: String) = {
    throw new RuntimeException(varName + " needs to be of " + correctType + " type")
  }

  private def handleGeneralTypeException(varName: String) = {
    throw new RuntimeException("The incoming type of " + varName + " has to of PIR internal DataType t")
  }
}
