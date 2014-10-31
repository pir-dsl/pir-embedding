package edu.uwm.cs.pir

trait DataType {

  //May need to use this let clause later
  def let[A, B](x: A)(f: A => B): B = f(x)

  // Basic types that can be used to represent types from heterogeneous systems
  type T
  //type Singleton = ()	
  //type ClosedRecord = ()
  //type Sequences[X] = List[X]
  type BaseType = Int | Char
  	
  //////////////////////////////////////////////////
  type ¬[T] = T => Nothing
  type ∨[T1, T2] = ¬[¬[T1] with ¬[T2]]
  type ¬¬[T] = ¬[¬[T]]
  type |[T1, T2] = { type λ[X] = ¬¬[X] <:< (T1 ∨ T2) }
  //////////////////////////////////////////////////
//  Example  
//    def size[T: (Int | String)#λ](t: T) = t match {
//      case i: Int => i
//      case s: String => s.length
//    }    
}