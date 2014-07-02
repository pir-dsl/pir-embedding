package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.IIndexable

trait IIndexableFeature extends IFeature {
  type Concrete[T] = IIndexable[T]
}