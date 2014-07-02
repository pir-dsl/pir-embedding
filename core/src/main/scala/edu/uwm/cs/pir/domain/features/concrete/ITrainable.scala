package edu.uwm.cs.pir.domain.features.concrete

trait ITrainable[T] extends IConcreteFeature[T] {
  type TrainableDoc
  def getTrainableDoc : TrainableDoc
}