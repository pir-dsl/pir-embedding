package edu.uwm.cs.pir.domain.features.concrete

trait ICustom[T] extends ITrainable[T] with IComparable[T] with IIndexable[T] {
	type NewT 
    def customFunc : NewT
}