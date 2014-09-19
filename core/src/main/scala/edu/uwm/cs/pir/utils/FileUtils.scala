package edu.uwm.cs.pir.utils

import java.io._
import java.net._
import java.util.Vector
import java.lang.reflect.Method

import org.apache.lucene.document.Document
import scala.collection.JavaConverters._
import cc.mallet.topics.ParallelTopicModel

import edu.uwm.cs.pir.utils.GeneralUtils._
import edu.uwm.cs.pir.spark.SparkObject._
import edu.uwm.cs.pir.utils.AWSS3API._

import edu.uwm.cs.mir.prototypes.model.CCARelevanceResult
import edu.uwm.cs.mir.prototypes.feature.utils.FeatureUtils

object FileUtils {
  
  def pathToFileList(url : String, modalityType : String) : List[String] = {
    lazy val list: List[String] = {
      if (url.endsWith(".jpg") || url.endsWith(".xml")) {
        //This is for the case of query or such
        List(url)
      } else {
        //TODO
        val ext =  if ("image".equals(modalityType)) "jpg" else "xml"
        //log("image url = " + url)
        if (awsS3Config.is_s3_storage) {
          //log("isIs_s3_storage = true")
          getIdList(url, ext)
        } else {
          //log("isIs_s3_storage = false")
          FeatureUtils.getFilenameListByPath(url, ext).asScala.toList
        }
      }
    }
    list
  }
  
  def addJarToClasspath(s: String) = {
    val f = new File(s)
    val u = f.toURI()
    val urlClassLoader = ClassLoader.getSystemClassLoader.asInstanceOf[URLClassLoader]
    val urlClass = classOf[URLClassLoader]
    val method = urlClass.getDeclaredMethod("addURL")
    method.setAccessible(true)
    method.invoke(urlClassLoader, Array[Any](u.toURL))
  }

  @throws(classOf[IOException])
  private def assignCategory(relevanceArray: Array[CCARelevanceResult], modalityType: String, config: AWSS3Config) = {
    var nameCategoryMap = createNameCategoryMap(modalityType, config)
    relevanceArray.foreach { ccaRelevanceResult => ccaRelevanceResult.setCategory(nameCategoryMap.get(ccaRelevanceResult.getName).get) }
  }

  @throws(classOf[IOException])
  def createNameCategoryMap(modalityType: String, config: AWSS3Config) = {
    var lines = List[String]()
    var categoryList = List[String]()
    val amazonS3Client = AWSS3API.getAmazonS3Client(config)
    if (config.is_s3_storage) {
      if (AWSS3API.checkObjectExists(config, "ground_truth/all_txt_img_cat.list", amazonS3Client, false)) {
        lines = AWSS3API.getS3ObjectAsLines(config, "ground_truth/all_txt_img_cat.list", amazonS3Client, false)
      } else {
        throw new RuntimeException("Cannot find AWS S3 id: " + "ground_truth/all_txt_img_cat.list")
      }

      if (AWSS3API.checkObjectExists(config, "ground_truth/categories.list", amazonS3Client, false)) {
        categoryList = AWSS3API.getS3ObjectAsLines(config, "ground_truth/categories.list", amazonS3Client, false)
      } else {
        throw new RuntimeException("Cannot find AWS S3 id: " + "ground_truth/categories.list")
      }
    } else {
      lines = org.apache.commons.io.FileUtils.readLines(new File(Constants.DATA_ROOT + "ground_truth/all_txt_img_cat.list")).asScala.toList
      categoryList = org.apache.commons.io.FileUtils.readLines(new File(Constants.DATA_ROOT + "ground_truth/categories.list")).asScala.toList
    }
    constructNameCategoryMap(lines, categoryList, modalityType)
  }

  @throws(classOf[Exception])
  def deSerializeObject(id: String, config: AWSS3Config, checkPersisted: Boolean) = {
    var is: java.io.InputStream = null

    if (config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(config)
      is = AWSS3API.getS3ObjectAsInputStream(config, id, amazonS3Client, checkPersisted)
    } else {
      is = new FileInputStream(id)
    }
    val in = new ObjectInputStream(is)
    val obj = in.readObject
    in.close
    obj
  }

  @throws(classOf[Exception])
  def deSerializeLDAModel(fileName: String, config: AWSS3Config): ParallelTopicModel = {
    var is: java.io.InputStream = null

    if (config.is_s3_storage) {
      //System.out.println("get model file from AWS")
      val amazonS3Client = AWSS3API.getAmazonS3Client(config)
      is = AWSS3API.getS3ObjectAsInputStream(config, fileName, amazonS3Client, false)
    } else {
      is = new FileInputStream(fileName)
    }
    val in = new ObjectInputStream(is)
    val model = in.readObject.asInstanceOf[ParallelTopicModel]
    in.close

    model
  }

  def fileExist(fileString: String) = {
    (new File(fileString)).exists()
  }

  def getAllFilePathAndName(directory: File, descendIntoSubDirectories: Boolean, validExtensionArray: Seq[String]): List[String] = {
    var resultList = List[String]()
    val f = directory.listFiles()
    f.map(file => {
      var validExtension = false
      validExtensionArray.foreach(extension =>
        validExtension = validExtension | file.getName.toLowerCase.endsWith(extension))
      if (file != null && (validExtension && !file.getName().startsWith("tn_"))) {
        try {
          resultList = file.getCanonicalPath() :: resultList
        } catch {
          case ex: IOException =>
            log(ex.getMessage)("Severe")
            null
        }
      }
      if (descendIntoSubDirectories && file.isDirectory()) {
        val tmp = getAllFilePathAndName(file, true, validExtensionArray)
        if (tmp != null) {
          resultList = tmp ::: resultList
        }
      }

    })
    resultList
  }

  @throws(classOf[Exception])
  def getFilePathAndNameList(path: String, validExtensionArray: Seq[String]) = {
    getAllFilePathAndName(new java.io.File(path), true, validExtensionArray)
  }

  def getLongestCategoryStringLength(categoryList: List[String]) = {
    var longest = 0
    categoryList.foreach(category => { if (category.length > longest) { longest = category.length } })
    longest
  }

  def getNext(imageFiles: Vector[String], indexThreads: Map[String, Boolean], started: Boolean, finished: Vector[Document]) = {
    if (imageFiles.size() < 1) {
      var fb = true
      for (t <- indexThreads.keySet) {
        fb = fb && indexThreads.get(t).get
      }
      if (started && fb) {
        null
      }
    }
    while (finished.size() < 1) {
      try {
        Thread.sleep(100)
      } catch {
        case ex: InterruptedException => ex.printStackTrace()
      }
    }
    finished.remove(0)
  }

  @throws(classOf[IOException])
  def readTextFile(path: String) = {
    org.apache.commons.io.FileUtils.readFileToString(new File(path))
  }

  @throws(classOf[IOException])
  def readTextFileLines(path: String) = {
    org.apache.commons.io.FileUtils.readLines(new File(path))
  }

  @throws(classOf[IOException])
  def serializeObject(obj: Any, config: AWSS3Config, id: String, checkPersisted: Boolean) = {
    if ((config.is_s3_storage)) {
      var result: Boolean = false
      val amazonS3Client = AWSS3API.getAmazonS3Client(config)
      if (AWSS3API.checkObjectExists(config, id, amazonS3Client, false)) {
        result = AWSS3API.deleteS3ObjectAsOutputStream(config, id, amazonS3Client, checkPersisted)
        if (!result) throw new RuntimeException("Cannnot delete AWS S3 file: " + id)
      }
      val is = getObjectInputStream(obj)
      result = AWSS3API.putS3ObjectAsOutputStream(config, id, is, amazonS3Client, checkPersisted)
      if (!result) throw new RuntimeException("Cannnot create AWS S3 file: " + id)
    } else {
      val file = new File(id)
      if (!((file).exists())) {
        file.createNewFile()
        serializeObjectToFile(obj, file)
      }
    }
  }

  @throws(classOf[IOException])
  def serializeObjectToFile(obj: Any, file: File) = {
    val fos = new FileOutputStream(file)
    val out = new ObjectOutputStream(fos)
    out.writeObject(obj)
    out.close()
  }

  private def constructIdCategoryMap(categoryList: List[String]) = {
    var idCategories = scala.collection.mutable.Map[String, String]()
    for (i <- 0 to categoryList.size) {
      idCategories.put((i + 1).toString, categoryList(i))
    }
    idCategories
  }

  private def constructNameCategoryMap(lines: List[String], categoryList: List[String], modalityType: String) = {
    val nameCategoryMap = scala.collection.mutable.Map[String, String]()
    val idCategoryMap = constructIdCategoryMap(categoryList)
    lines.foreach(line => {
      val items = line.split("\t")
      nameCategoryMap.put((if ("text".equals(modalityType)) items(0) else items(1)), idCategoryMap(items(2)))
    })
    nameCategoryMap
  }

  //throws IOException
  def storeObject(obj: Object, config: AWSS3Config, id: String, checkPersisted: Boolean): Unit = {
    if ((config.is_s3_storage)) {
      var result = false
      val amazonS3Client = AWSS3API.getAmazonS3Client(config)
      if (AWSS3API.checkObjectExists(config, id, amazonS3Client, false)) {
        result = AWSS3API.deleteS3ObjectAsOutputStream(config, id, amazonS3Client, checkPersisted)
        if (!result) throw new RuntimeException("Cannnot delete AWS S3 file: " + id)
      }
      var is: InputStream = getObjectInputStream(obj)
      result = AWSS3API.putS3ObjectAsOutputStream(config, id, is, amazonS3Client, checkPersisted)
      if (!result) throw new RuntimeException("Cannnot create AWS S3 file: " + id)
    } else {
      val file = new File(id)
      if (!((file).exists)) {
        file.createNewFile
        serializeObjectToFile(obj, file)
      }
    }
  }

  @throws(classOf[IOException])
  def getObjectInputStream(obj: Any) : InputStream = {
    val os = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(os)
    oos.writeObject(obj)
    oos.flush()
    oos.close()
    val is: InputStream = new ByteArrayInputStream(os.toByteArray())
    is
  }
  
  def loadObject(id: String, config: AWSS3Config, checkPersisted: Boolean): Object = {
    val is = if (config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(config)
      AWSS3API.getS3ObjectAsInputStream(config, id, amazonS3Client, checkPersisted)
    } else {
      new FileInputStream(id)
    }
    val in = new ClassLoaderObjectInputStream(is)
    val obj = in.readObject
    in.close
    obj
  }
}