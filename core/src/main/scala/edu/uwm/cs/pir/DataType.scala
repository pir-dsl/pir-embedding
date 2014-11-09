package edu.uwm.cs.pir

trait DataType {

  // Basic types that can be used to represent types from heterogeneous systems
  trait t
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
  //val testV : BaseType = new BaseType(Right(Left(1)))
  //new OpenRecord(testV , null)
  sealed class SequenceType(val value: List[r[t | EmptyWord]]) extends t {}
  sealed class BaseType(val value: Either[Char, Either[Int, Double]]) extends t {}
  sealed class SpecialType(value: Either[Null, Either[Nothing, Any]]) extends t {}
  sealed class UnionType[X1 <: t, X2 <: t](val x1: X1, x2: X2) extends t {}

  sealed class EmptyWord {
    val value = ""
  }
  sealed class r[T: (t | EmptyWord)#or]() {
    def value[T: (t | EmptyWord)#or](v: T) = v match {
      case i: EmptyWord => i
      case i: t => i
    }
  }
  sealed class r_*[T: (t | EmptyWord)#or](val values: List[r[t | EmptyWord]] = List[r[t | EmptyWord]]()) extends r[T] {}
  sealed class r_+[T: (t | EmptyWord)#or](val first : r[t | EmptyWord], val values: List[r[t | EmptyWord]] = List[r[t | EmptyWord]]()) extends r[T] {}
  sealed class rr[T: (t | EmptyWord)#or](val left : r[t | EmptyWord], val right : r[t | EmptyWord]) extends r[T] {}
  sealed class r_or_r[T: (t | EmptyWord)#or](val v1 : r[t | EmptyWord], val v2 : r[t | EmptyWord]) extends r[T] {}
  sealed class r_?[T: (t | EmptyWord)#or](val undefined : Nothing, val value : r[t | EmptyWord])
  
  type ^[T] = T => Nothing
  type v[T1, T2] = ^[^[T1] with ^[T2]]
  type ^^[T] = ^[^[T]]
  type or[X, T1, T2] = ^^[X] <:< (T1 v T2)
  type |[T1, T2] = { type or[X] = ^^[X] <:< (T1 v T2) }

  //  trait Disj[T] {
  //    type or[S] = Disj[T with ^[S]]
  //    type apply = ^[T]
  //  }
  //  type disj[T] = { type or[S] = Disj[^[T]]#or[S] }
  //
  //  ////////////////////////////////////////////////////

  //  def let[A, B](x: A)(f: A => B): B = f(x)
  ////////////////////////////////////////////////////
}