package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.IConcreteFeature
import edu.uwm.cs.pir.domain.features.concrete.IHistogram

trait IHistogramFeature extends IFeature {
  type T = Int
  type Concrete[T] = IHistogram[Int]
}