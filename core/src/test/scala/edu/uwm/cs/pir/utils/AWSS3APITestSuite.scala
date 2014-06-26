package edu.uwm.cs.pir.utils

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
import edu.uwm.cs.mir.prototypes.utils.Utils

import java.io.ByteArrayInputStream
import java.io.InputStream

import net.semanticmetadata.lire.clustering.Cluster
import net.semanticmetadata.lire.utils.SerializationUtils

@RunWith(classOf[JUnitRunner])
class AWSS3APITestSuite extends FunSuite with BeforeAndAfter {
  var config: AWSS3Config = _
  before {
    config = new AWSS3Config(true, true, "PirData", "", "cachedData")
  }

  test("putS3ObjectAsOutputStream") {
    val is = new ByteArrayInputStream("Test File for Fun!".getBytes)
    val id = "text_model/test.txt"
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val idExists = AWSS3API.checkObjectExists(config, id, amazonS3Client, false)
    if (!idExists) {
      val result = AWSS3API.putS3ObjectAsOutputStream(config, id, is, amazonS3Client, false)
      assert(result)
    }
  }

  test("deleteS3ObjectAsOutputStream") {
    val id = "text_model/test.txt"
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val idExists = AWSS3API.checkObjectExists(config, id, amazonS3Client, false)
    if (idExists) {
      val result = AWSS3API.deleteS3ObjectAsOutputStream(config, id, amazonS3Client, false)
      assert(result)
    }
  }

  test("getS3ObjectAsString") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val categories = AWSS3API.getS3ObjectAsString(config, "ground_truth/categories.list", amazonS3Client, false)
    assert(categories != null)
    println("categories: \n" + categories)
  }

  test("getS3ObjectAsStringArray") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val stopWords = AWSS3API.getS3ObjectAsString(config, "stoplists/en.txt", amazonS3Client, false)
    val array = stopWords.split("\r\n|\r|\n")
    array.foreach(println(_))
  }

  test("getNumberOfLinesOfS3Objects") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val numOfLines = AWSS3API.getNumberOfLinesOfS3Objects(config, "ground_truth/categories.list", amazonS3Client, false)
    assert(10 == numOfLines)
  }

  test("getS3ObjectAsURL") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val url = AWSS3API.getS3ObjectAsURL(config, "samples/images/training/ceb47321a83dd824cec2d5d3f2034765.jpg", amazonS3Client, false)
    assert(url != null)
    println("url = " + url)
    val image = Utils.readInputAsImage(url)
    assert(image != null)
  }

  test("getS3ObjectAsURI") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val uri = AWSS3API.getS3ObjectAsURI(config, "stoplists/en.txt", amazonS3Client, false)
    assert(uri != null)
    println("uri = " + uri)
  }

  test("getS3ObjectAsInpuStream_iaprtc12") {
    config.bucket_name = "iaprtc12"
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val is = AWSS3API.getS3ObjectAsInputStream(config, "images/01/1000.jpg", amazonS3Client, false)
    assert(is != null)
    val image = Utils.readInputAsImage(is)
    assert(image != null)
    config.bucket_name = "PirData"
  }

  test("getS3ObjectAsInpuStream") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val is = AWSS3API.getS3ObjectAsInputStream(config, "samples/images/training/ceb47321a83dd824cec2d5d3f2034765.jpg", amazonS3Client, false)
    assert(is != null)
    val image = Utils.readInputAsImage(is)
    assert(image != null)
  }

  test("getModelS3Object") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    val is = AWSS3API.getS3ObjectAsInputStream(config, "image_cluster/clusters.ser", amazonS3Client, false)
    assert(is != null)
    val clusters = readClusters(is)
    assert((clusters.length > 0))
  }

  test("checkObjectExists") {
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    var result = AWSS3API.checkObjectExists(config, "image_cluster/clusters.ser", amazonS3Client, false)
    assert(result)
    result = AWSS3API.checkObjectExists(config, "text_features/ALL_DESC.txt", amazonS3Client, false)
    assert(result)
    result = AWSS3API.checkObjectExists(config, "text_model/lda_model.ser", amazonS3Client, false)
    assert(result)
  }

  def readClusters(is: InputStream) = {
    var tmp = new Array[Byte](4)
    is.read(tmp, 0, 4)
    val result = new Array[Cluster](SerializationUtils.toInt(tmp))
    tmp = new Array[Byte](128 * 4)
    result.foreach { elem =>
      {
        val bytesRead = is.read(tmp, 0, 128 * 4)
        if (bytesRead != 128 * 4) println("Didn't read enough bytes ...")
        //elem = new Cluster()
        elem.setByteRepresentation(tmp)
      }
    }
    is.close
    result
  }

}