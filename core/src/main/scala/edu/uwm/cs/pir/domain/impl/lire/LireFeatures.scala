package edu.uwm.cs.pir.domain.impl.lire

import edu.uwm.cs.pir.Constants
import edu.uwm.cs.pir.domain._
import edu.uwm.cs.pir.utils.GeneralUtils._
import edu.uwm.cs.pir.utils.IndexUtils
import edu.uwm.cs.pir.utils.AWSS3API
import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
import edu.uwm.cs.pir.utils.ImageUtils
import edu.uwm.cs.pir.utils.FileUtils

import java.awt.image.BufferedImage
import java.io.IOException
import java.io.InputStream

import com.amazonaws.services.s3.AmazonS3

import scala.collection.JavaConverters._

//TODO: Need to refactor the below
import edu.uwm.cs.mir.prototypes.feature.lire.utils.LireFeatureUtils
import edu.uwm.cs.mir.prototypes.feature.utils.FeatureUtils

trait FeatureContainer[X]
trait ContainerFeature { type Feature[X] = FeatureContainer[X] }

case class Location(val url: String, val awsS3Config: AWSS3Config)

class LireImage(val p: Location) extends FeatureContainer[LireImage] {
  def getFeature: BufferedImage = {
    var image: BufferedImage = null
    try {
      if (p.awsS3Config.is_s3_storage) {
        println("get image from AWS S3")
        val amazonS3Client = AWSS3API.getAmazonS3Client(p.awsS3Config)
        val is = AWSS3API.getS3ObjectAsInputStream(p.awsS3Config, p.url, amazonS3Client, false)
        println("is=" + is)
        image = ImageUtils.readInputAsImage(is)
        println("image=" + image)
      } else {
        println("get image from file system")
        image = ImageUtils.readInputAsImage(p.url)
      }
    } catch {
      case ex: IOException => new RuntimeException(ex)

    }
    image
  }

  //override def compareTo(other: LireImage) = 1
}

case class LireText(val p: Location) extends FeatureContainer[LireText] {
  def getFeature: String = {
    var text: String = null
    if (p.awsS3Config.is_s3_storage) {
      val amazonS3Client = AWSS3API.getAmazonS3Client(p.awsS3Config)
      text = AWSS3API.getS3ObjectAsString(p.awsS3Config, p.url, amazonS3Client, false)
    } else {
      //println("get text from file system")
      try {
        text = FileUtils.readTextFile(p.url)
      } catch {
        case ex: IOException => new RuntimeException(ex)
      }
    }
    text
  }
}

trait LireFeature {
  def getFeature(image: LireImage, scaleWidth: Int, scaleHeight: Int) : BufferedImage = {
    var bufferedImage = image.getFeature
    try {
      if (bufferedImage != null) {
        val imageType = if (bufferedImage.getType == 0) BufferedImage.TYPE_INT_ARGB else bufferedImage.getType
        bufferedImage = LireFeatureUtils.resizeImageWithHint(bufferedImage, imageType, scaleWidth, scaleHeight)
      }
    } catch {
      //When fail, we print out the exception and continue to process
      case ex: Exception => println(ex.getMessage())
    }
    bufferedImage
  }
}

case class CEDDWrapper(cedd: net.semanticmetadata.lire.imageanalysis.CEDD) extends FeatureContainer[CEDDWrapper]
case class LireCEDD(val scaleWidth: Int, val scaleHeight: Int) extends LireFeature with FeatureContainer[LireCEDD] {
  def getFeature(image: LireImage): CEDDWrapper = {
    val bufferedImage = super.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.CEDD
    lireFeature.extract(bufferedImage)
    CEDDWrapper(lireFeature)
  }
}

case class FCTHWrapper(fcth: net.semanticmetadata.lire.imageanalysis.FCTH) extends FeatureContainer[FCTHWrapper]
case class LireFCTH(val scaleWidth: Int, val scaleHeight: Int) extends LireFeature with FeatureContainer[LireFCTH] {
  def getFeature(image: LireImage): FCTHWrapper = {
    val bufferedImage = super.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.FCTH
    lireFeature.extract(bufferedImage)
    FCTHWrapper(lireFeature)
  }
}

case class ColorLayoutWrapper(cedd: net.semanticmetadata.lire.imageanalysis.ColorLayout) extends FeatureContainer[ColorLayoutWrapper]
case class LireColorLayout(val scaleWidth: Int, val scaleHeight: Int) extends LireFeature with FeatureContainer[LireColorLayout] {
  def getFeature(image: LireImage): ColorLayoutWrapper = {
    val bufferedImage = super.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.ColorLayout
    lireFeature.extract(bufferedImage)
    ColorLayoutWrapper(lireFeature)
  }
}

case class EdgeHistogramWrapper(edgeHistogram: net.semanticmetadata.lire.imageanalysis.EdgeHistogram) extends FeatureContainer[EdgeHistogramWrapper]
case class LireEdgeHistogram(val scaleWidth: Int, val scaleHeight: Int) extends LireFeature with FeatureContainer[LireEdgeHistogram] {
  def getFeature(image: LireImage): EdgeHistogramWrapper = {
    val bufferedImage = super.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.EdgeHistogram
    lireFeature.extract(bufferedImage)
    EdgeHistogramWrapper(lireFeature)
  }
}

case class GaborWrapper(gabor: net.semanticmetadata.lire.imageanalysis.Gabor) extends FeatureContainer[GaborWrapper]
case class LireGabor(val scaleWidth: Int, val scaleHeight: Int) extends LireFeature with FeatureContainer[LireGabor] {
  def getFeature(image: LireImage): GaborWrapper = {
    val bufferedImage = super.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.Gabor
    lireFeature.extract(bufferedImage)
    GaborWrapper(lireFeature)
  }
}

import net.semanticmetadata.lire.imageanalysis.sift.Feature
import net.semanticmetadata.lire.imageanalysis.sift.Extractor
case class SIFTWrapper(features: List[Feature]) extends FeatureContainer[SIFTWrapper]
case class LireSIFT() {
  def getFeature(image: LireImage, scaleWidth: Int = Constants.SCALE_WIDTH, scaleHeight: Int = Constants.SCALE_HEIGHT): SIFTWrapper = {
    var features: List[Feature] = null
    try {
      var bufferedImage = image.getFeature
      // When this is null, the jpg file cannot be processed
      if (bufferedImage != null) {
        val imageType = if (bufferedImage.getType == 0) BufferedImage.TYPE_INT_ARGB else bufferedImage.getType
        bufferedImage = LireFeatureUtils.resizeImageWithHint(bufferedImage, imageType, scaleWidth, scaleHeight)
        features = (new Extractor).computeSiftFeatures(net.semanticmetadata.lire.utils.ImageUtils.scaleImage(bufferedImage, scaleWidth, scaleHeight)).asScala.toList
        //Sort by descending order
        features = features.sortWith((feature1, feature2) => if (feature1.scale < feature2.scale) true else false)
      }
    } catch {
      case ex: Exception => { log(ex.getMessage)("error"); throw new RuntimeException("image id = " + image.p.url + ", " + ex.getMessage) }
    }
    SIFTWrapper(features)
  }
}

import net.semanticmetadata.lire.utils.LuceneUtils
import org.apache.lucene.document.Document
import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexWriter
case class LuceneDocumentWrapper(document: Document) extends FeatureContainer[LuceneDocumentWrapper]
case class LireIndex() {
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def getFeature(location: String, features: List[List[LuceneDocumentWrapper]]): String = {
    val indexWriter = LuceneUtils.createIndexWriter(location, true)
    features.foreach(featureList => featureList.foreach(feature => {
      IndexUtils.addOneDocToExistingIndex(indexWriter, feature.document)
    }))
    indexWriter.close()
    location
  }
}

case class LireQuery() {

}

  