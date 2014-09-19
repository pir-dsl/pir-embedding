package edu.uwm.cs.pir.domain.impl.mallet

import edu.uwm.cs.pir.utils.Constants._
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Reader
import java.io.UnsupportedEncodingException
import java.util.Arrays
import java.util.HashMap
import java.util.Map
import java.util.Map.Entry
import java.util.Set
import java.util.ArrayList
import java.util.LinkedList
import java.util.Map
import java.util.regex.Pattern
import java.text.NumberFormat
import java.text.DecimalFormat
import org.apache.commons.io.FileUtils

import cc.mallet.pipe.CharSequence2TokenSequence
import cc.mallet.pipe.CharSequenceLowercase
import cc.mallet.pipe.Pipe
import cc.mallet.pipe.SerialPipes
import cc.mallet.pipe.TokenSequence2FeatureSequence
import cc.mallet.pipe.TokenSequenceRemoveStopwords
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.Instance
import cc.mallet.types.InstanceList

import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.SparseDoubleMatrix2D

import edu.uwm.cs.mir.prototypes.feature.Histogram
import edu.uwm.cs.mir.prototypes.feature.IFeature
import edu.uwm.cs.mir.prototypes.feature.LdaFeature
import edu.uwm.cs.mir.prototypes.model.CCARelevanceResult
import edu.uwm.cs.mir.prototypes.model.CCAResults
import edu.uwm.cs.mir.prototypes.model.CCA
import edu.uwm.cs.mir.prototypes.model.CCAModel
import edu.uwm.cs.mir.prototypes.model.CCAOptions
import edu.uwm.cs.mir.prototypes.model.CCAResults
import edu.uwm.cs.mir.prototypes.proj.wikipedia.utils.WikiPediaDataSetUtils
import edu.uwm.cs.mir.prototypes.train.utils.XY2DArrayContainer
import edu.uwm.cs.mir.prototypes.aws.AWSS3Config

import edu.uwm.cs.mir.prototypes.feature.wikipedia.WikiPediaTextAdaptor
import edu.uwm.cs.mir.prototypes.model.LdaModel
import edu.uwm.cs.mir.prototypes.feature.LdaFeature
import edu.uwm.cs.mir.prototypes.proj.LocalTokenSequenceRemoveStopwords
import edu.uwm.cs.mir.prototypes.utils.Utils

import net.semanticmetadata.lire.imageanalysis.sift.Feature

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.impl.SimpleAssociation
import edu.uwm.cs.pir.domain.Training
import scala.collection.JavaConverters._

trait MalletTraining extends Training with SimpleAssociation {
  var numberOfClusters: Int
  var clusterFilename: String
  var ldaModelFile: String
  var descFile: String
  var stopwordFile: String
  var numberOfTopics: Int
  var alphaSum: Double
  var betaW: Double
  var numberOfSamplers: Int
  var numberOfIterations: Int
  var encoding: String

  var gibbsSamplingIteration: Int
  var gibbsSamplingThinning: Int
  var gibbsSamplingBurnin: Int

  //The below temporary workaround needs to be fixed
  val config = new AWSS3Config

  var imageFeatureSize: Int
  var textFeatureSize: Int

  var numberOfTopResults: Int
  var groungTruthcategories: String

  type X = List[net.semanticmetadata.lire.imageanalysis.sift.Feature] //edu.uwm.cs.mir.prototypes.feature.lire.SiftFeatureAdaptor
  type Histogram = edu.uwm.cs.mir.prototypes.feature.Histogram
  type Cluster = edu.uwm.cs.mir.prototypes.model.ClusterModel

  type Lda_Input = WikiPediaTextAdaptor
  type Topic = LdaModel

  type Distribution = LdaFeature

  type CCA_Input_X = LdaFeature
  type CCA_Input_Y = Histogram
  type CCA = CCAModel
  type CCAResult = String

  val groundTruthMap = getGroundTruthMapping("all_txt_img_cat.list", "training", config)
  groundTruthMap.asScala.foreach(elem => idMap += (elem._1.asInstanceOf[ID] -> elem._2.asInstanceOf[ID]))

  //override def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y]) => ID = ???
  /**/

  def f_cluster_train: TrnOp[X, Cluster] = {
    s: List[(ID, X)] =>
      {
        var fileExists = false
        //TODO: fix Aamazon data access
        //	if (this.config.isIs_s3_storag) {
        //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
        //	    fileExists = AWSS3API.checkObjectExists(config, clusterFilePathAndName, amazonS3Client, false)
        //	} else {
        val existingSerFile = new File(clusterFilename)
        fileExists = existingSerFile.exists
        //}

        if (fileExists) {
          println("Cluster File " + clusterFilename + " exists. Quit serialization")
        } else {

          // find the documents for building the vocabulary:
          val k = new net.semanticmetadata.lire.clustering.KMeans(numberOfClusters)
          // fill the KMeans object:
          val features = new LinkedList[Array[Double]]
          s.foreach(
            elem => {
              features.clear
              if (elem._2 != null) {
                //TODO: See if we can avoid this
                val id = elem._1.asInstanceOf[String]
                val newFeatures = elem._2
                if (newFeatures != null) {
                  for (feature <- newFeatures) {
                    val f = new Feature
                    f.setByteArrayRepresentation(feature.getByteArrayRepresentation)
                    features.add(f.descriptor)
                  }
                  //System.out.println(", features=" + features)
                  k.addImage(id, features)
                }
              }
            })
          // do the clustering:
          println("k.getFeatureCount = " + k.getFeatureCount)
          println("Starting clustering ...")
          k.init
          println("Step.")
          var time = System.currentTimeMillis
          var laststress = k.clusteringStep

          println(getDuration(time) + " -> Next step.")
          time = System.currentTimeMillis
          var newStress = k.clusteringStep

          // critical part: Give the difference in between steps as a
          // constraint for accuracy vs. runtime trade off.
          val threshold = Math.max(20d, k.getFeatureCount / 1000d)
          println("Threshold = " + threshold)
          val df = NumberFormat.getNumberInstance.asInstanceOf[DecimalFormat]
          df.setMaximumFractionDigits(3)
          while (Math.abs(newStress - laststress) > threshold) {
            println(getDuration(time) + " -> Next step. Stress difference ~ |" + newStress + " - " + laststress + "| = " + df.format(Math.abs(newStress - laststress)))
            time = System.currentTimeMillis
            laststress = newStress
            newStress = k.clusteringStep
          }
          // Serializing clusters to a file on the disk ...
          val clusters = k.getClusters
          try {
            net.semanticmetadata.lire.clustering.Cluster.writeClusters(clusters, clusterFilename)
          } catch {
            case e: IOException => throw new RuntimeException(e.getMessage)
          }

          //TODO: fix this one
          //	    boolean result
          //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
          //	    if (config.isIs_s3_storage) {
          //		if (AWSS3API.checkObjectExists(config, clusterFilePathAndName, amazonS3Client, false)) {
          //		   result = AWSS3API.deleteS3ObjectAsOutputStream(config, clusterFilePathAndName, amazonS3Client, false)
          //		   if (!result) throw new RuntimeException("Cannot delete cluster file: " + clusterFilePathAndName)
          //		}
          //		AWSS3API.putS3ObjectAsFile(config, clusterFilePathAndName, new File(clusterFilePathAndName), amazonS3Client, false)

          //May consider delete this file as it has been pushed to AWS S3
          //new File(clusterFilePathAndName).delete
          //}
        }
        new edu.uwm.cs.mir.prototypes.model.ClusterModel(clusterFilename)
      }
  }

  private def getDuration(time: Double): String = {
    val min = (System.currentTimeMillis - time) / (1000 * 60)
    val sec = (min - Math.floor(min)) * 60
    "" + min + ":" + sec
  }

  private def getClusters(model: edu.uwm.cs.mir.prototypes.model.ClusterModel): Array[net.semanticmetadata.lire.clustering.Cluster] = {
    var clusters: Array[net.semanticmetadata.lire.clustering.Cluster] = null
    try {
      val filename = model.getClusterFilename

      //TODO: Added this later
      //	    if (this.config.isIs_s3_storage) {
      //		AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
      //		InputStream is = AWSS3API.getS3ObjectAsInpuStream(this.config, filename, amazonS3Client, false)
      //		clusters = readClusters(is)
      //	    } else {
      clusters = net.semanticmetadata.lire.clustering.Cluster.readClusters(filename)
      //}
    } catch {
      case e: IOException => throw new RuntimeException(e.getMessage)
    }
    clusters
  }

  /**
   * Find the appropriate cluster for a given feature.
   *
   * @param f
   * @return the index of the cluster.
   */
  private def clusterForFeature(f: Feature, clusters: Array[net.semanticmetadata.lire.clustering.Cluster]): Int = {
    var d = clusters(0).getDistance(f)
    var tmp = d
    var result = 0
    for (i <- 1 to clusters.length - 1) {
      tmp = clusters(i).getDistance(f)
      if (tmp < d) {
        d = tmp
        result = i
      }
    }
    result
  }

  def f_cluster_proj: DPrjOp[X, Histogram, Cluster] = {
    (x, cluster) =>
      {
        val clusters = getClusters(cluster)
        val histogram = new Array[Double](numberOfClusters)
        val features = x
        if (features != null) {
          for (feature <- features) {
            val index = clusterForFeature(feature, clusters)
            histogram(index) = histogram(index) + 1
          }
        }
        //TODO: See if we can avoid this
        val id = "/ID.JPG" //x.asInstanceOf[String]
        val newId =
          if (!edu.uwm.cs.mir.prototypes.utils.OSChecker.isWindows) {
            id.substring(id.lastIndexOf("/") + 1, id.lastIndexOf("."))
          } else {
            id.substring(id.lastIndexOf("\\") + 1, id.lastIndexOf("."))
          }
        (new edu.uwm.cs.mir.prototypes.feature.Histogram(newId, histogram)).asInstanceOf[Histogram]
      }
  }

  def f_lda_train: TrnOp[Lda_Input, Topic] = {

    s: List[(ID, Lda_Input)] =>
      {
        constructDescFile(s.map(elem => elem._2))
        var model: ParallelTopicModel = null
        val instances = getInstanceList(stopwordFile)
        try {
          println("ldaModelFile = " + ldaModelFile)
          val file = new File(ldaModelFile)
          if (!((file).exists())) {
            model = getLDAModel(descFile, instances)
            Utils.serializeObject(model, config, ldaModelFile, false)
          }
        } catch {
          case e: Exception => throw new RuntimeException(e.getMessage)
        }
        println("total instances = " + instances.size)
        (new LdaModel(ldaModelFile)).asInstanceOf[Topic]
      }
  }

  private def constructDescFile(featureList: List[WikiPediaTextAdaptor]): Unit = {
    val sb = new StringBuffer
    try {
      val nameCategoryMap = createNameCategoryMap("text", config)
      for (wikiPediaTextAdaptor <- featureList) {
        val filePathAndName = wikiPediaTextAdaptor.getIntegratedTextLocation
        // The below appears not needed in Windows 7
        // String fileName = (!OSChecker.isWindows) ?
        // (filePathAndName.substring(filePathAndName.lastIndexOf("/")
        // + 1, filePathAndName.lastIndexOf("."))) :
        // (filePathAndName.substring(filePathAndName.lastIndexOf("\\")
        // + 1, filePathAndName.lastIndexOf(".")))
        val fileName = filePathAndName.substring(filePathAndName.lastIndexOf("/") + 1, filePathAndName.lastIndexOf("."))
        sb.append(fileName + "\t" + nameCategoryMap.get(fileName) + "\t" + FileUtils.readFileToString(new File(filePathAndName), encoding) + "\n")
      }
    } catch {
      case (e: Exception) => println("WARN: File processing problem due to " + e.getMessage)
    }
    println("Total line number = " + featureList.length)

    try {
      Utils.serializeObject(sb.toString, config, descFile, false)
    } catch {
      case e: IOException => throw new RuntimeException
    }
  }

  private def createPipeList(stopListFilePathAndName: String): ArrayList[Pipe] = {
    // Begin by importing documents from text to feature sequences
    val pipeList = new ArrayList[Pipe]

    // Pipes: lowercase, tokenize, remove stopwords, map to features
    pipeList.add(new CharSequenceLowercase)
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))

    //TODO: fix this
    //	if ((this.config.isIs_s3_storage)) {
    //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
    //	    String content = AWSS3API.getS3ObjectAsString(config, stopListFilePathAndName, amazonS3Client, false)
    //	    pipeList.add(new LocalTokenSequenceRemoveStopwords(content.split("\r\n|\r|\n"), "UTF-8", false, false, false))
    //	} else {
    val file = new File(stopListFilePathAndName)
    file.getParentFile.mkdirs
    pipeList.add(new TokenSequenceRemoveStopwords(file, "UTF-8", false, false, false))
    //}
    pipeList.add(new TokenSequence2FeatureSequence)
    pipeList
  }

  @throws(classOf[UnsupportedEncodingException])
  @throws(classOf[FileNotFoundException])
  private def fillInstances(filePathAndName: String, instances: InstanceList): Unit = {
    val is: InputStream = new FileInputStream(new File(filePathAndName))
    //TODO: Fix the below
    //	if ((this.config.isIs_s3_storage)) {
    //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
    //	    is = AWSS3API.getS3ObjectAsInpuStream(config, filePathAndName, amazonS3Client, false)
    //	} else {
    //is = new FileInputStream(new File(filePathAndName))
    //}
    val fileReader = new InputStreamReader(is, "UTF-8")
    instances.addThruPipe(new CsvIterator(fileReader, Pattern.compile("^(\\S*)[\\s,]*(\\S*)[\\s,]*(.*)$"), 3, 2, 1))
  }

  private def getInstanceList(stopListFilePathAndName: String): InstanceList = {
    val pipeList = createPipeList(stopListFilePathAndName)
    val instances = new InstanceList(new SerialPipes(pipeList))
    instances
  }

  @throws(classOf[UnsupportedEncodingException])
  @throws(classOf[FileNotFoundException])
  @throws(classOf[IOException])
  private def getLDAModel(filePathAndName: String, instances: InstanceList): ParallelTopicModel = {
    fillInstances(filePathAndName, instances)
    val model = new ParallelTopicModel(numberOfTopics, alphaSum, betaW)
    model.addInstances(instances)
    // Use given number of parallel samplers, which each look at one half the corpus and combine statistics after every iteration.
    model.setNumThreads(numberOfSamplers)

    // Run the model for 50 iterations and stop (this is for testing only, for real applications, use 1000 to 2000 iterations)
    model.setNumIterations(numberOfIterations)
    model.estimate
    model
  }

  def f_lda_proj: DPrjOp[Lda_Input, Distribution, Topic] = {
    (x, topic) =>
      {
        val ldaModelFile = topic.getLdaModelFileLocation
        var id: String = null
        var ldaProbabilities: Array[Double] = null
        var wikiPediaTextAdaptor: WikiPediaTextAdaptor = null
        var instance: Instance = null
        try {
          val instanceList = getInstanceList(stopwordFile)
          wikiPediaTextAdaptor = x
          id = wikiPediaTextAdaptor.getId
          var text = wikiPediaTextAdaptor.getText
          instance = new Instance(text, null, "query instance", null)
          instanceList.addThruPipe(instance)
          ldaProbabilities = getParallelTopicModel(ldaModelFile).getInferencer.getSampledDistribution(instanceList.get(0), gibbsSamplingIteration, gibbsSamplingThinning, gibbsSamplingBurnin)
          instanceList.remove(instance)
        } catch {
          case e: Exception => {
            println("id = " + id + ", instance = " + instance + ", error message = " + e.getMessage)
            ldaProbabilities = new Array[Double](10)
          }
        }
        id = id.substring(id.lastIndexOf("/") + 1, id.lastIndexOf("."))
        new LdaFeature(id, ldaProbabilities)
      }
  }

  @throws(classOf[Exception])
  private def getParallelTopicModel(ldaModelFile: String): ParallelTopicModel = {
    //TODO: add code to make value cached when it's already loaded
    Utils.deSerializeLDAModel(ldaModelFile, config)
  }

  def f_cca_train: TrnOp[(CCA_Input_X, CCA_Input_Y), CCA] = {

    s: List[(ID, (CCA_Input_X, CCA_Input_Y))] =>
      {

        var ccaResults: CCAResults = null
        val siftHistogramMap = new HashMap[String, Array[Double]]
        val ldaFeatureMap = new HashMap[String, Array[Double]]

        s.foreach(elem => {
          val histogram = elem._2._2
          val ldaFeature = elem._2._1
          val idString = elem._1.asInstanceOf[String]
          if (histogram != null) {
            siftHistogramMap.put(idString.substring(idString.lastIndexOf(":") + 1, idString.length), histogram.getFeature)
          } else {
            siftHistogramMap.put("nullSIFTId", new Array[Double](imageFeatureSize))
          }

          if (ldaFeature != null) {
            ldaFeatureMap.put(idString.substring(0, idString.lastIndexOf(":")), ldaFeature.getFeature)
          } else {
            ldaFeatureMap.put("nullLDAId", new Array[Double](textFeatureSize))
          }
        })

        if ((siftHistogramMap != null) && (ldaFeatureMap != null)) {
          var container: XY2DArrayContainer = null
          try {
            container = constructXY2DArray(ldaFeatureMap, siftHistogramMap)
          } catch {
            case e: IOException => throw new RuntimeException(e)
          }
          ccaResults = computeCCAResults(container)
        }
        new CCAModel(ccaResults, ldaFeatureMap, siftHistogramMap)
      }
  }

  private def computeCCAResults(container: XY2DArrayContainer): CCAResults = {
    val ccaOptions = new CCAOptions
    val XMatrix = new SparseDoubleMatrix2D(container.getX)
    val YMatrix = new SparseDoubleMatrix2D(container.getY)
    val ccaResults = edu.uwm.cs.mir.prototypes.model.CCA.computeCCA(XMatrix, YMatrix, ccaOptions)
    ccaResults
  }

  @throws(classOf[IOException])
  private def constructXY2DArray(ldaFeatureMap: Map[String, Array[Double]], siftHistogramMap: Map[String, Array[Double]]): XY2DArrayContainer = {
    //val groundTruthMap = getGroundTruthMapping("all_txt_img_cat.list", "training", config)
    if (siftHistogramMap.size != ldaFeatureMap.size) {
      println("The numbers of samples in X and Y are not equal, where siftHistogramMap size = " + siftHistogramMap.size + ", ldaFeatureMap size = " + ldaFeatureMap.size)
      var diffKeyArray: java.util.List[String] = new ArrayList[String]
      if (siftHistogramMap.size < ldaFeatureMap.size) {
        diffKeyArray = getDiffKeyArray(siftHistogramMap, ldaFeatureMap, groundTruthMap)
        for (key <- diffKeyArray.asScala) {
          siftHistogramMap.put(key, new Array[Double](imageFeatureSize))
        }
      } else if (siftHistogramMap.size > ldaFeatureMap.size) {
        diffKeyArray = getDiffKeyArray(ldaFeatureMap, siftHistogramMap, groundTruthMap)
        for (key <- diffKeyArray.asScala) {
          ldaFeatureMap.put(key, new Array[Double](textFeatureSize))
        }
      }
      println("diffKeyArray" + diffKeyArray)
    }

    val container = new XY2DArrayContainer
    val X = new Array[Array[Double]](siftHistogramMap.size)
    val Y = new Array[Array[Double]](ldaFeatureMap.size)
    var firstDimension = 0
    for (entry <- ldaFeatureMap.entrySet.asScala) {
      val key = entry.getKey
      X(firstDimension) = ldaFeatureMap.get(key)
      // Y[firstDimension++] =
      // addHistogramPadding(siftHistogramMap.get(groundTruthMap.get(key)))
      Y(firstDimension) = siftHistogramMap.get(groundTruthMap.get(key.substring(0, key.lastIndexOf(".xml"))) + ".jpg")
      firstDimension += 1
    }
    container.setX(X)
    container.setY(Y)
    container
  }

  @throws(classOf[IOException])
  private def getGroundTruthMapping(groundTruthFile: String, datasetType: String, config: AWSS3Config): java.util.Map[String, String] = {
    val groundTruthMap = new HashMap[String, String]

    var lines: java.util.List[String] = null
    //TODO: fix this
    //	if (config.isIs_s3_storage()) {
    //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
    //	    lines = AWSS3API.getS3ObjectAsLines(config, "ground_truth/" + groundTruthFile, amazonS3Client, false);
    //	} else {
    lines = FileUtils.readLines(new File(SAMPLES_ROOT + "ground_truth/" + groundTruthFile))
    //	}
    for (line <- lines.asScala) {
      val items = line.split("\t")
      groundTruthMap.put(items(0), items(1))
    }
    groundTruthMap
  }

  // groundTruthMap is a map from key text filename (no path, no extension) to value image filename
  private def getDiffKeyArray(siftHistogramMap: Map[String, Array[Double]], ldaFeatureMap: Map[String, Array[Double]], groundTruthMap: Map[String, String]): java.util.List[String] = {
    val diffKeyArray = new ArrayList[String]
    val keys1 = siftHistogramMap.keySet
    val keys2 = ldaFeatureMap.keySet

    for (key2 <- keys2.asScala) {
      val key1 = groundTruthMap.get(key2)
      if (!keys1.contains(key1)) {
        diffKeyArray.add(key1)
      }
    }
    diffKeyArray
  }

  case class Result(val relevanceArray: Array[CCARelevanceResult] = null, val outputNeeded: Boolean = false)

  def f_cca_proj1: DPrjOp[CCA_Input_X, List[(ID, CCAResult)], CCA] = {
    (histogram, model) =>
      {
        var result = new Result
        try {
          result = queryAgainstCCA(false, histogram.getFeature, model)
        } catch {
          case e: Exception => throw new RuntimeException(e)
        }
        assignCategory(result.relevanceArray, "image", config)
        if (result.outputNeeded) {
          try {
            val list = outputRelevances(result.relevanceArray, "image")
            list.foreach(elem => println(elem._1 + ":" + elem._2))
            list
          } catch {
            case e: IOException => throw new RuntimeException(e)
          }
        } else {
          val list = List()
          println(list)
          list
        }
      }
  }

  @throws(classOf[IOException])
  private def outputRelevances(relevanceArray: Array[CCARelevanceResult], modality: String): List[(ID, String)] = {
    relevanceArray.sorted
    val header = "\nMatching filename |  Category | Relevance score\n"
    if (relevanceArray != null) {
      numberOfTopResults = if (numberOfTopResults <= relevanceArray.length) numberOfTopResults else relevanceArray.length
      var categoryList: java.util.List[String] = null
      //TODO: fix this
      //	    if (config.isIs_s3_storage) {
      //		AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
      //		categoryList = AWSS3API.getS3ObjectAsLines(config, groungTruthcategories, amazonS3Client, false)
      //	    }
      //	    else {
      categoryList = FileUtils.readLines(new File(groungTruthcategories))
      //}
      val longestCategoryStringLength = Utils.getLongestCategoryStringLength(categoryList)
      //TODO: The below may need to be modified for ID
      relevanceArray.map(elem =>
        {
          (elem.getName.asInstanceOf[ID], elem.toString(modality, longestCategoryStringLength))
        }).toList
    } else List()
  }

  @throws(classOf[IOException])
  private def assignCategory(relevanceArray: Array[CCARelevanceResult], modalityType: String, config: AWSS3Config): Unit = {
    val nameCategoryMap = createNameCategoryMap(modalityType, config)
    for (ccaRelevanceResult <- relevanceArray) {
      ccaRelevanceResult.setCategory(nameCategoryMap.get(ccaRelevanceResult.getName))
    }
  }

  @throws(classOf[IOException])
  private def createNameCategoryMap(modalityType: String, config: AWSS3Config): java.util.Map[String, String] = {
    var lines: java.util.List[String] = new ArrayList[String]
    var categoryList: java.util.List[String] = new ArrayList[String]
    //TODO: fix this
    //	AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config)
    //	if (config.isIs_s3_storage) {
    //	    if (AWSS3API.checkObjectExists(config, "ground_truth/all_txt_img_cat.list", amazonS3Client, false)) {
    //		lines = AWSS3API.getS3ObjectAsLines(config, "ground_truth/all_txt_img_cat.list", amazonS3Client, false)
    //	    } else {
    //		throw new RuntimeException("Cannot find AWS S3 id: " + "ground_truth/all_txt_img_cat.list")
    //	    }
    //	    
    //	    if (AWSS3API.checkObjectExists(config, "ground_truth/categories.list", amazonS3Client, false)) {
    //		categoryList = AWSS3API.getS3ObjectAsLines(config, "ground_truth/categories.list", amazonS3Client, false)
    //	    } else {
    //		throw new RuntimeException("Cannot find AWS S3 id: " + "ground_truth/categories.list")
    //	    }
    //	} else {
    lines = FileUtils.readLines(new File(SAMPLES_ROOT + "ground_truth/all_txt_img_cat.list"))
    categoryList = FileUtils.readLines(new File(SAMPLES_ROOT + "ground_truth/categories.list"))
    //}
    constructNameCategoryMap(lines, categoryList, modalityType)
  }

  private def constructNameCategoryMap(lines: java.util.List[String], categoryList: java.util.List[String], modalityType: String): java.util.Map[String, String] = {
    val nameCategoryMap = new HashMap[String, String]
    val idCategoryMap = constructIdCategoryMap(categoryList)
    for (line <- lines.asScala) {
      val items = line.split("\t")
      nameCategoryMap.put((if ("text".equals(modalityType)) items(0) else items(1)), idCategoryMap.get(items(2)))
    }
    nameCategoryMap
  }

  private def constructIdCategoryMap(categoryList: java.util.List[String]): java.util.Map[String, String] = {
    val idCategoryMap = new HashMap[String, String]
    for (i <- 0 to categoryList.asScala.length - 1) {
      idCategoryMap.put(Integer.toString((i + 1)), categoryList.get(i))
    }
    idCategoryMap
  }

  @throws(classOf[Exception])
  private def queryAgainstCCA(isImageProj: Boolean, queryVector: Array[Double], model: CCAModel): Result = {

    val siftFeatureMap = model.getSiftHistogramMap
    val ldaFeatureMap = model.getLdaFeatureMap

    if ((siftFeatureMap == null) || (ldaFeatureMap == null)) {
      throw new Exception("Either of the feature map " + siftFeatureMap + "/" + ldaFeatureMap + " is empty")
    }

    val existingVectors = if (isImageProj) ldaFeatureMap else siftFeatureMap
    val ccaResults = model.getCcaResults
    val relevanceArray = CCA.getCrossModalRelevances(queryVector, existingVectors, ccaResults, if (isImageProj) "Y" else "X")
    assignCategory(relevanceArray, if (isImageProj) "text" else "image", config)
    val result = new Result(relevanceArray, true)
    result
  }

  def f_cca_proj2: DPrjOp[CCA_Input_Y, List[(ID, CCAResult)], CCA] = {
    (ldaFeature, model) =>
      {
        var result = new Result
        try {
          result = queryAgainstCCA(true, ldaFeature.getFeature, model)
        } catch {
          case e: Exception => throw new RuntimeException(e)
        }
        assignCategory(result.relevanceArray, "text", config)
        if (result.outputNeeded) {
          try {
            outputRelevances(result.relevanceArray, "text")
          } catch {
            case e: IOException => throw new RuntimeException(e)
          }
        } else List()
      }
  }
}