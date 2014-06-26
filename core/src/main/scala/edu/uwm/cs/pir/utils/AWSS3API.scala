package edu.uwm.cs.pir.utils

import edu.uwm.cs.pir.spark.SparkObject._

import scala.collection.JavaConversions._

import java.io.File
import java.io.IOException
import java.io.InputStream
import java.net.MalformedURLException
import java.net.URI
import java.net.URL

import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.S3Object
import com.amazonaws.services.s3.model.ListObjectsRequest
import com.amazonaws.services.s3.model.DeleteObjectRequest
import com.amazonaws.services.s3.model.GetObjectRequest
import com.amazonaws.services.s3.model.ObjectMetadata
import com.amazonaws.services.s3.model.PutObjectRequest
import com.amazonaws.services.s3.model.S3ObjectInputStream

import edu.uwm.cs.pir.utils.GeneralUtils._

object AWSS3API {

  class AWSS3Config(var is_s3_storage: Boolean, var s3_disable_get_object_md5_validation: Boolean,
    var bucket_name: String, var s3_root: String, var s3_persistence_bucket_name: String)

  def getIdList(prefix: String, extension: String, checkPersisted: Boolean = false): List[String] = {
    System.setProperty("com.amazonaws.services.s3.disableGetObjectMD5Validation", if (awsS3Config.is_s3_storage) "true" else "false")
    val bucketName = if (!checkPersisted) awsS3Config.bucket_name else awsS3Config.s3_persistence_bucket_name
    val amazonS3Client = new AmazonS3Client
    val request = new ListObjectsRequest().withBucketName(bucketName).withPrefix(prefix)
    var allResultRetrieved = false
    var keyList = List[String]()
    do {
      val response = amazonS3Client.listObjects(request)
      val summaries = response.getObjectSummaries
      keyList = keyList ::: {
        val newSummaries = summaries.map(summary => summary.getKey)
        if (!extension.isEmpty) newSummaries.filter(key => { /*log("key = " + key)("INFO"); */ key.endsWith(extension) }).toList else newSummaries.toList
      }
      if (response.isTruncated) {
        request.setMarker(response.getNextMarker)
      } else {
        allResultRetrieved = true
      }
    } while (!allResultRetrieved)
    keyList
  }

  def isExistingS3Location(S3String: String, hostname: String = ""): Boolean = {
    val amazonS3Client = getAmazonS3Client(awsS3Config)
    try {
      val result = checkObjectExists(awsS3Config, S3String + (if (hostname.isEmpty) "" else "/" + hostname), amazonS3Client, true)
      log("isExistingS3Location=" + result)("INFO")
      result
    } catch {
      case _: Throwable => false
    }
  }

  def getExistingSignatureId(S3String: String): String = {
    val amazonS3Client = getAmazonS3Client(awsS3Config)
    try {
      val result = checkObjectExists(awsS3Config, S3String, amazonS3Client, true)
      log("hasExistingS3Location=" + result)("INFO")
      if (result) S3String else ""
    } catch {
      case _: Throwable => ""
    }
  }

  val md5ValidationString = "com.amazonaws.services.s3.disableGetObjectMD5Validation"

  def checkObjectExists(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean): Boolean = {
    var obj: S3Object = null
    try {
      obj = amazonS3Client.getObject(if (checkPersisted) { config.s3_persistence_bucket_name } else { config.bucket_name }, id)
    } catch {
      case ex: Exception => false
    }
    val result = (obj != null)
    try {
      obj.getObjectContent.close
    } catch {
      case ex: IOException => throw new RuntimeException(ex)
    }
    result
  }

  def getS3ObjectAsString(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {

    val is = getS3ObjectAsInputStream(config, id, amazonS3Client, checkPersisted)
    val resultString = convertStreamToString(is)
    try {
      is.close
    } catch {
      case ex: IOException => throw new RuntimeException("Cannot close the input stream for id: " + id, ex)
    }
    resultString
  }

  def getNumberOfLinesOfS3Objects(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = getS3ObjectAsLines(config, id, amazonS3Client, checkPersisted).size
  

  def putS3ObjectAsOutputStream(config: AWSS3Config, id: String, is: InputStream, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    val request = new PutObjectRequest(if (checkPersisted) config.s3_persistence_bucket_name else config.bucket_name, id, is, new ObjectMetadata)
    try {
      amazonS3Client.putObject(request)
    } catch {
      case ex: Exception => { println(ex.getMessage); false }
    }
    true
  }

  def putS3ObjectAsFile(config: AWSS3Config, id: String, file: File, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    val request = new PutObjectRequest(if (checkPersisted) config.s3_persistence_bucket_name else config.bucket_name, id, file)
    try {
      amazonS3Client.putObject(request)
    } catch {
      case ex: Exception => { println(ex.getMessage); false }
    }
    true
  }

  def deleteS3ObjectAsOutputStream(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    val request = new DeleteObjectRequest(if (checkPersisted) config.s3_persistence_bucket_name else config.bucket_name, id)
    try {
      amazonS3Client.deleteObject(request)
    } catch {
      case ex: Exception => { println(ex.getMessage); false }
    }
    true
  }

  def getS3ObjectAsInputStream(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = getS3ObjectInputStream(config, id, amazonS3Client, checkPersisted)

  def getS3ObjectAsURL(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    try {
      getS3ObjectAsURI(config, id, amazonS3Client, checkPersisted).toURL
    } catch {
      case ex: MalformedURLException => throw new RuntimeException(ex)
    }
  }

  def getS3ObjectAsURI(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    val s3ObjectInputStream = getS3ObjectInputStream(config, id, amazonS3Client, checkPersisted)
    s3ObjectInputStream.getHttpRequest.getURI
  }

  def getS3ObjectInputStream(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    val rangeObjectRequest = new GetObjectRequest(if (checkPersisted) config.s3_persistence_bucket_name else config.bucket_name, id)
    val objectPortion = amazonS3Client.getObject(rangeObjectRequest)
    objectPortion.getObjectContent
  }

  def getAmazonS3Client(config: AWSS3Config) = {
    System.setProperty(md5ValidationString, if (config.s3_disable_get_object_md5_validation) "true" else "false")
    new AmazonS3Client
  }

  def getS3ObjectAsLines(config: AWSS3Config, id: String, amazonS3Client: AmazonS3, checkPersisted: Boolean) = {
    getS3ObjectAsString(config, id, amazonS3Client, checkPersisted).split("\r\n|\r|\n").toList
  }

  def convertStreamToString(is: InputStream) = {
    val s = new java.util.Scanner(is).useDelimiter("\\A")
    if (s.hasNext) s.next else ""
  }

}