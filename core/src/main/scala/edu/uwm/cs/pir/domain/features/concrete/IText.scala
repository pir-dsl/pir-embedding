package edu.uwm.cs.pir.domain.features.concrete

trait IText[T] extends IConcreteFeature[T] {

}

class Text(val id : String) extends IText[String] {
  
}