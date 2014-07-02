package edu.uwm.cs.pir.domain.features.concrete

trait IText[T] extends IConcreteFeature[T] {

}

class Text[T] (val text : T) extends IText[T] {
  override def getFeature : T = text
}