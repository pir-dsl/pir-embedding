package edu.uwm.cs.pir.domain.features.concrete

trait IImage[T] extends IConcreteFeature[T] {
	
}

class Image(val id : Location) extends IImage[Location] {
	
}
