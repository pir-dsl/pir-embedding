package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.ITrainable

trait ITrainableFeature extends IFeature {
  type Concrete[T] = ITrainable[T]
}