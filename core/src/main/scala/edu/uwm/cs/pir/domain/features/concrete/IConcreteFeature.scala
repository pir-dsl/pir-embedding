package edu.uwm.cs.pir.domain.features.concrete

import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
case class Location(val url: String, val awsS3Config: AWSS3Config)

trait IConcreteFeature[T] {
}