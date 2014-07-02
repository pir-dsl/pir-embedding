package edu.uwm.cs.pir.domain.features.concrete

trait IIndexable[T] extends IConcreteFeature[T] {
	type IndexableDoc
	def getIndexableDoc : IndexableDoc
}