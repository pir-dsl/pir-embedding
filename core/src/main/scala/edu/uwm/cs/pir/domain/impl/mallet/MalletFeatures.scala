package edu.uwm.cs.pir.domain.impl.mallet

import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Reader
import java.io.UnsupportedEncodingException
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
import edu.uwm.cs.mir.prototypes.feature.wikipedia.WikiPediaTextAdaptor
import edu.uwm.cs.mir.prototypes.model.LdaModel
import edu.uwm.cs.mir.prototypes.feature.LdaFeature
import edu.uwm.cs.mir.prototypes.proj.LocalTokenSequenceRemoveStopwords
import edu.uwm.cs.mir.prototypes.utils.Utils

import net.semanticmetadata.lire.imageanalysis.sift.Feature

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.Training
import scala.collection.JavaConverters._

trait MalletTraining extends Training {
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
  val config = new edu.uwm.cs.mir.prototypes.aws.AWSS3Config

  type X = List[net.semanticmetadata.lire.imageanalysis.sift.Feature] //edu.uwm.cs.mir.prototypes.feature.lire.SiftFeatureAdaptor
  type Histogram = edu.uwm.cs.mir.prototypes.feature.Histogram
  type Cluster = edu.uwm.cs.mir.prototypes.model.ClusterModel

  type Lda_Input = WikiPediaTextAdaptor
  type Topic = LdaModel

  type Distribution = LdaFeature

  def f_cluster_train: TrnOp[X, Cluster] = {
    s: List[(ID, X)] =>
      {
        var fileExists = false
        //TODO: fix Aamazon data access
        //	if (this.config.isIs_s3_storage()) {
        //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
        //	    fileExists = AWSS3API.checkObjectExists(config, clusterFilePathAndName, amazonS3Client, false);
        //	} else {
        val existingSerFile = new File(clusterFilename)
        fileExists = existingSerFile.exists()
        //}

        if (fileExists) {
          println("Cluster File " + clusterFilename + " exists. Quit serialization")
        } else {

          // find the documents for building the vocabulary:
          val k = new net.semanticmetadata.lire.clustering.KMeans(numberOfClusters)
          // fill the KMeans object:
          val features = new LinkedList[Array[Double]]()
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
                    f.setByteArrayRepresentation(feature.getByteArrayRepresentation())
                    features.add(f.descriptor)
                  }
                  //System.out.println(", features=" + features);
                  k.addImage(id, features);
                }
              }
            })
          // do the clustering:
          println("k.getFeatureCount() = " + k.getFeatureCount)
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
            println(getDuration(time) + " -> Next step. Stress difference ~ |" + newStress.asInstanceOf[Integer] + " - " + laststress.asInstanceOf[Integer] + "| = " + df.format(Math.abs(newStress - laststress)))
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
          //	    boolean result;
          //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
          //	    if (config.isIs_s3_storage()) {
          //		if (AWSS3API.checkObjectExists(config, clusterFilePathAndName, amazonS3Client, false)) {
          //		   result = AWSS3API.deleteS3ObjectAsOutputStream(config, clusterFilePathAndName, amazonS3Client, false);
          //		   if (!result) throw new RuntimeException("Cannot delete cluster file: " + clusterFilePathAndName); 
          //		}
          //		AWSS3API.putS3ObjectAsFile(config, clusterFilePathAndName, new File(clusterFilePathAndName), amazonS3Client, false);

          //May consider delete this file as it has been pushed to AWS S3
          //new File(clusterFilePathAndName).delete();
          //}
        }
        new edu.uwm.cs.mir.prototypes.model.ClusterModel(clusterFilename)
      }
  }

  private def getDuration(time: Double): String = {
    val min = (System.currentTimeMillis() - time) / (1000 * 60)
    val sec = (min - Math.floor(min)) * 60
    String.format("%02d:%02d", min.asInstanceOf[Integer], sec.asInstanceOf[Integer])
  }

  private def getClusters(model: edu.uwm.cs.mir.prototypes.model.ClusterModel): Array[net.semanticmetadata.lire.clustering.Cluster] = {
    var clusters: Array[net.semanticmetadata.lire.clustering.Cluster] = null
    try {
      val filename = model.getClusterFilename();

      //TODO: Added this later
      //	    if (this.config.isIs_s3_storage()) {
      //		AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
      //		InputStream is = AWSS3API.getS3ObjectAsInpuStream(this.config, filename, amazonS3Client, false);
      //		clusters = readClusters(is);
      //	    } else {
      clusters = net.semanticmetadata.lire.clustering.Cluster.readClusters(filename)
      //}
    } catch {
      case e: IOException => throw new RuntimeException(e.getMessage())
    }
    clusters;
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
    for (i <- 1 to clusters.length) {
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
        val histogram = new Array[Double](numberOfClusters);
        val features = x
        if (features != null) {
          for (feature <- features) {
            val index = clusterForFeature(feature, clusters)
            histogram(index) = histogram(index) + 1
          }
        }
        //TODO: See if we can avoid this
        val id = x.asInstanceOf[String]
        val newId =
          if (!edu.uwm.cs.mir.prototypes.utils.OSChecker.isWindows()) {
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
          model = getLDAModel(descFile, instances)
          println("ldaModelFile = " + ldaModelFile)
          Utils.serializeObject(model, config, ldaModelFile, false)
        } catch {
          case e: Exception => throw new RuntimeException(e.getMessage)
        }
        println("total instances = " + instances.size)
        (new LdaModel(ldaModelFile)).asInstanceOf[Topic]
      }
    //??? // List[(ID, X)] => Topic[X]
  }

  private def constructDescFile(featureList: List[WikiPediaTextAdaptor]): Unit = {
    val sb = new StringBuffer
    try {
      val nameCategoryMap = Utils.createNameCategoryMap("text", config)
      for (wikiPediaTextAdaptor <- featureList) {
        val filePathAndName = wikiPediaTextAdaptor.getIntegratedTextLocation()
        // The below appears not needed in Windows 7
        // String fileName = (!OSChecker.isWindows()) ?
        // (filePathAndName.substring(filePathAndName.lastIndexOf("/")
        // + 1, filePathAndName.lastIndexOf("."))) :
        // (filePathAndName.substring(filePathAndName.lastIndexOf("\\")
        // + 1, filePathAndName.lastIndexOf(".")));
        val fileName = filePathAndName.substring(filePathAndName.lastIndexOf("/") + 1, filePathAndName.lastIndexOf("."))
        sb.append(fileName + "\t" + nameCategoryMap.get(fileName) + "\t" + FileUtils.readFileToString(new File(filePathAndName), encoding) + "\n")
      }
    } catch {
      case (e: Exception) => println("WARN: File processing problem due to " + e.getMessage());
    }
    println("Total line number = " + featureList.length);

    try {
      Utils.serializeObject(sb.toString(), config, descFile, false)
    } catch {
      case e: IOException => throw new RuntimeException()
    }
  }

  private def createPipeList(stopListFilePathAndName: String): ArrayList[Pipe] = {
    // Begin by importing documents from text to feature sequences
    val pipeList = new ArrayList[Pipe]()

    // Pipes: lowercase, tokenize, remove stopwords, map to features
    pipeList.add(new CharSequenceLowercase());
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")));

    //TODO: fix this
    //	if ((this.config.isIs_s3_storage())) {
    //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
    //	    String content = AWSS3API.getS3ObjectAsString(config, stopListFilePathAndName, amazonS3Client, false);
    //	    pipeList.add(new LocalTokenSequenceRemoveStopwords(content.split("\r\n|\r|\n"), "UTF-8", false, false, false));
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
    //	if ((this.config.isIs_s3_storage())) {
    //	    AmazonS3 amazonS3Client = AWSS3API.getAmazonS3Client(config);
    //	    is = AWSS3API.getS3ObjectAsInpuStream(config, filePathAndName, amazonS3Client, false);
    //	} else {
    //is = new FileInputStream(new File(filePathAndName));
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
    model.estimate()
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
          ldaProbabilities = getParallelTopicModel(ldaModelFile).getInferencer().getSampledDistribution(instanceList.get(0), gibbsSamplingIteration, gibbsSamplingThinning, gibbsSamplingBurnin)
          instanceList.remove(instance)
        } catch {
          case e: Exception => {
            println("id = " + id + ", instance = " + instance + ", error message = " + e.getMessage())
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

  def f_cca_train[X, Y]: TrnOp[(X, Y), CCA[X, Y]] = ??? // (List[(ID, (X, Y))]) => CCA[X, Y]
  def f_cca_proj1[X, Y]: DPrjOp[X, List[ID], CCA[X, Y]] = ??? // (X, CCA[X, Y]) => List[ID]
  def f_cca_proj2[X, Y]: DPrjOp[Y, List[ID], CCA[X, Y]] = ??? // (Y, CCA[X, Y]) => List[ID]
}