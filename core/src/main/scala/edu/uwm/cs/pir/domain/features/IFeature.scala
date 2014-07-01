package edu.uwm.cs.pir.domain.features

trait IFeature {
    type T; type T1 <: T; type T2 <: T
	type Concrete[T]
	def f_getFeature : Concrete[T]
	def f_compare (other : IFeature) : Boolean = false
	def f_transform (in : Concrete[T1]) : Concrete[T2]
}
 