package edu.uwm.cs.pir.utils

import java.io.File
import java.io.InputStream
import com.amazonaws.services.s3.AmazonS3Client
import edu.uwm.cs.pir.utils.AWSS3API._
import edu.uwm.cs.pir.spark.SparkObject._

import net.semanticmetadata.lire.clustering.Cluster

trait Resource[T] {
  def readResourceAsInputStream(r: T, input: String): InputStream
//  def readResourceAsString(r: T, input: String): String
//  def readResourceAsStringList(r: T, input: String): List[String]  
  def saveResource(r: T, input: String): Boolean
  def deleteResource(r: T, input: String): Boolean
  def exists(r: T, input: String): Boolean
}

object Resource {
  implicit object ResourceFile extends Resource[String] {
    
    
    def readResourceAsInputStream(r: String, input: String): InputStream = {
     //No-op
     null
    }
  
//    def readResourceAsString(r: String, input: String): String = {
//      new File(input)
//    }
//    
//    def readResourceAsStringList(r: String, input: String): List[String]  = {
//      
//    }

    def saveResource(r: String, input: String = null): Boolean = {
      //no-op
      true
    }
    def deleteResource(r: String, input: String = null): Boolean = false
    def exists(r: String, input: String = null): Boolean = {
      new File(r).exists
    }
  }

  implicit object ResourceAmazonS3 extends Resource[AmazonS3Client] {
    def readResourceAsInputStream(r: AmazonS3Client, input: String): InputStream = {
       AWSS3API.getS3ObjectAsInputStream(awsS3Config, input, r, false)
    }
  
//    def readResourceAsString(r: AmazonS3Client, input: String): String = {
//      AWSS3API.getS3ObjectAsString(awsS3Config, input, r, false)
//    }
//    
//    def readResourceAsStringList(r: AmazonS3Client, input: String): List[String]  = {
//      AWSS3API.getS3ObjectAsLines(awsS3Config, input, r, false);
//    }

    def saveResource(r: AmazonS3Client, input : String): Boolean = {
      if (AWSS3API.checkObjectExists(awsS3Config, input, r, false)) {
        val result = AWSS3API.deleteS3ObjectAsOutputStream(awsS3Config, input, r, false)
        if (!result) throw new RuntimeException("Cannot delete cluster file: " + r /*clusterFilePathAndName*/ )
      }
      AWSS3API.putS3ObjectAsFile(awsS3Config, input, new File(input), r, false)
    }
    def deleteResource(r: AmazonS3Client, input: String): Boolean = false
    def exists(r: AmazonS3Client, input : String): Boolean = {
      AWSS3API.checkObjectExists(awsS3Config, input, r, false)
    }
  }
}

object ResourceAPI {

  def main(args: Array[String]) = {
    //println(resourceExists("Test"))
    //println(resourceSave("Test"))
    println(clusterRead("clusters.ser"))
  }

  def clusterRead(source : String): Array[Cluster] = {
    if (awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      val is = resource_read(amazonS3Client, source)
      readClusters(is)
    } else {
      //no-op
      Cluster.readClusters(source)
    }
  }

  private def readClusters(is : InputStream) : Array[Cluster] = {
    new Array[Cluster](0)
  }
  
  private def resource_read[T](source : T, input : String)(implicit r : Resource[T]) : InputStream = {
    r.readResourceAsInputStream(source, input)
  }
  
  def resourceSave(source : String): Boolean = {
    if (awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      resource_save(amazonS3Client, source)
    } else {
      //no-op
      true
    }
  }
  
  private def resource_save[T](source : T, input : String)(implicit r : Resource[T]) : Boolean = {
    r.saveResource(source, input)
  }
  
  def resourceDelete[T](source : T, input: String)(implicit r: Resource[T]): Boolean = r.deleteResource(source, input)
  
  def resourceExists(source : String, input : String = null): Boolean = {
    if (awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(awsS3Config)
      resource_exists(amazonS3Client, source)
    } else resource_exists(source, input)
  }
  
  private def resource_exists[T](source : T, input : String)(implicit r : Resource[T]) : Boolean = {
    r.exists(source, input)
  }
  
  def exists[T](source : T, input: String)(implicit r: Resource[T]): Boolean = r.exists(source, input)

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

