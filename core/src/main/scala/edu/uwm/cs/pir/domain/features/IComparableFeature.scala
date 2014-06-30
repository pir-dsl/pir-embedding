package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.IComparable

trait IComparableFeature extends IFeature {
  type Concrete[T] = IConcreteFeature[T]
  override def f_transform (in: Concrete[T1]): IComparable[T2]
}