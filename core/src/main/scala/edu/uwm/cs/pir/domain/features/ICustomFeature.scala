package edu.uwm.cs.pir.domain.features

import edu.uwm.cs.pir.domain.features.concrete.ICustom

trait ICustomFeature extends IFeature {
	type Concrete[T] = ICustom[T]
}