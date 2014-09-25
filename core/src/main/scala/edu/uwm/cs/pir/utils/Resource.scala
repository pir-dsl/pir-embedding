package edu.uwm.cs.pir.utils

import java.io.File
import edu.uwm.cs.pir.utils.AWSS3API._
import edu.uwm.cs.pir.spark.SparkObject._

trait Resource[T] {
  def deleteResource(r: T): Boolean
  def exists(r: T): Boolean
}

object Resource {
  implicit object ResourceFile extends Resource[File] {
    def deleteResource(r: File): Boolean = false
    def exists(r: File): Boolean = {
      r.exists
    }
  }

  implicit object ResourceAmazonS3 extends Resource[String] {
    def deleteResource(r: String): Boolean = false
    def exists(r: String): Boolean = {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      AWSS3API.checkObjectExists(awsS3Config, r, amazonS3Client, false)
    }
  }
}

object ResourceAPI {

  def main(args: Array[String]) = {
    println(resourceExists("Test"))
  }

  def resourceDelete[T](input: T)(implicit r: Resource[T]): Boolean = r.deleteResource(input)
  def resourceExists (filename : String) = {
    if (awsS3Config.is_s3_storage) exists(filename) else exists(new File(filename))
  }
  def exists[T](input: T)(implicit r: Resource[T]): Boolean = r.exists(input)

}

sealed trait TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] <: Up
}

class TTrue extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
}

class TFalse extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
}

