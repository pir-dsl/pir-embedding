package edu.uwm.cs.pir.domain.features

trait IFeature {
    type T
	type Concrete[T]
	def f_getFeature : Concrete[T]
	def f_compare (other : IFeature) : Boolean
}
 