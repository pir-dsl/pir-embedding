package edu.uwm.cs.pir.domain.features.concrete

trait IComparable[T] extends IConcreteFeature[T] {
	def compareTo(other : IComparable[T]) : Boolean
}