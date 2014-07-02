package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.IComparable

trait IComparableFeature extends IFeature {
  type Concrete[T] = IComparable[T]
}