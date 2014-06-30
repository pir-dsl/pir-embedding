package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.Histogram

trait IHistogramFeature extends IFeature {
  type T = Int; type T1 = Int; type T2 = Int
  type Concrete[T] = IConcreteFeature[Int]
  override def f_transform (in: Concrete[T1]) : Histogram[T2]
}