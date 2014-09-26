package edu.uwm.cs.pir.utils

import java.io.File
import com.amazonaws.services.s3.AmazonS3
import edu.uwm.cs.pir.utils.AWSS3API._
import edu.uwm.cs.pir.spark.SparkObject._

trait Resource[T] {
  def readResource(r: T, input: String = null): Any
  def saveResource(r: T, input: String = null): Boolean
  def deleteResource(r: T, input: String = null): Boolean
  def exists(r: T, input: String = null): Boolean
}

object Resource {
  implicit object ResourceFile extends Resource[String] {
    def readResource(r: String, input: String = null): Any = {
      //
    }

    def saveResource(r: String, input: String = null): Boolean = {
      //no-op
      false
    }
    def deleteResource(r: String, input: String = null): Boolean = false
    def exists(r: String, input: String = null): Boolean = {
      new File(r).exists
    }
  }

  implicit object ResourceAmazonS3 extends Resource[AmazonS3] {
    def readResource(r: AmazonS3, input: String): Any = {
      //
    }

    def saveResource(r: AmazonS3, input : String): Boolean = {
      var result: Boolean = false
      if (AWSS3API.checkObjectExists(awsS3Config, input, r, false)) {
        result = AWSS3API.deleteS3ObjectAsOutputStream(awsS3Config, input, r, false)
        if (!result) throw new RuntimeException("Cannot delete cluster file: " + r /*clusterFilePathAndName*/ )
      }
      AWSS3API.putS3ObjectAsFile(awsS3Config, input, new File(input), r, false)
    }
    def deleteResource(r: AmazonS3, input: String): Boolean = false
    def exists(r: AmazonS3, input : String): Boolean = {
      AWSS3API.checkObjectExists(awsS3Config, input, r, false)
    }
  }
}

object ResourceAPI {

  def main(args: Array[String]) = {
    println(resourceExists("Test"))
  }

  def resourceRead[T](input: T)(implicit r: Resource[T]): Any = {
    r.readResource(input)
  }

  def resourceSave[T](source : T, input: String = null)(implicit r: Resource[T]): Boolean = {
    if (awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      r.saveResource(amazonS3Client.asInstanceOf[T], input)
    } else {
      //no-op
      true
    }
  }
  
  def resourceDelete[T](input: T)(implicit r: Resource[T]): Boolean = r.deleteResource(input)
  
  def resourceExists[T](source : T, input : String = null)(implicit r: Resource[T]): Boolean = {
    if (awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      r.exists(amazonS3Client.asInstanceOf[T], input)
    } else r.exists(source, input)
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

