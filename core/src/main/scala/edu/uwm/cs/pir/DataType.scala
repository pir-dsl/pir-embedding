package edu.uwm.cs.pir

trait DataType {

  // Basic types that can be used to represent types from heterogeneous systems
  type DataType[T <: r] = Singleton[T] | Record[T] | Sequences[T] | BaseType | SpecialType | Union[T, T]
  type Singleton[T] = T
  type Sequences[T <: r] = Seq[T]
  type BaseType = disj[Int]#or[Float]#or[String]#apply
  type SpecialType = Any | Nothing | Null
  type Union[T1 <: r, T2 <: r] = T1 | T2
  sealed class Record[T <: r](l: DataType[T], r: DataType[T]) {
    def value = (l, r)
  }

  trait r
  class r_*(x: BaseType) extends r {

  }
  //The regex definition
  //  type r = DataType[BaseType]
  //  type r_*[T <: r] = Nothing | Seq[r]
  //  type r_+[T <: r] = r | Seq[r]
  //  type rr[T1 <: r, T2 <: r] = Seq[r]
  //  type r_|[T1 <: r, T2 <: r] = Seq[T1] | Seq[T2]

  trait Disj[T] {
    type or[S] = Disj[T with ^[S]]
    type apply = ^[T]
  }
  type disj[T] = { type or[S] = Disj[^[T]]#or[S] }

  ////////////////////////////////////////////////////
  type ^[T] = T => Nothing
  type v[T1, T2] = ^[^[T1] with ^[T2]]
  type ^^[T] = ^[^[T]]
  type or[X, T1, T2] = ^^[X] <:< (T1 v T2)

  type |[T1, T2] = T1 v T2 //{ type or[X] = ^^[X] <:< (T1 v T2) }
  ////////////////////////////////////////////////////

  //  //Example  
//    def size(t: BaseType) = t match {
//      case i: Int => i
//      case s: Char => 1
//    }
//    size(1)
//    size(' ')
  //  //May need to use this let clause later
  //  def let[A, B](x: A)(f: A => B): B = f(x)
}