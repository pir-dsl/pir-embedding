package edu.uwm.cs.pir.domain.features.concrete

trait IImage[T] extends IConcreteFeature[T] {}

class Image[T] (image : T) extends IImage[T] {
  override def getFeature : T = image
}

