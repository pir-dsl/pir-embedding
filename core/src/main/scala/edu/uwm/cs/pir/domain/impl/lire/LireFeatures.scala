package edu.uwm.cs.pir.domain.impl.lire

import edu.uwm.cs.pir._
import edu.uwm.cs.pir.domain._
import edu.uwm.cs.pir.utils.GeneralUtils._
import edu.uwm.cs.pir.utils.IndexUtils
import edu.uwm.cs.pir.utils.AWSS3API
import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
import edu.uwm.cs.pir.utils.ImageUtils
import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.domain.features._
import edu.uwm.cs.pir.domain.features.concrete._

import java.awt.image.BufferedImage
import java.io.IOException
import java.io.InputStream

import com.amazonaws.services.s3.AmazonS3

import scala.collection.JavaConverters._

//TODO: Need to refactor the below
import edu.uwm.cs.mir.prototypes.feature.lire.utils.LireFeatureUtils

//trait FeatureContainer[X]
//trait ContainerFeature { type Feature[X] = FeatureContainer[X] }

class LireImage(val p: Location) extends IFeature with IImage[Location] {
  type T = BufferedImage
  type Concrete[T] = IImage[BufferedImage]

  def f_getFeature: IImage[BufferedImage] = {
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
    new Image(image)
  }

  //override def compareTo(other: LireImage) = 1
}

case class LireText(val p: Location) extends IFeature with IText[Location] {
  def getFeature: IText[String] = {
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
    new Text(text)
  }
}

object FeatureUtils {
  def getFeature(image: LireImage, scaleWidth: Int, scaleHeight: Int): BufferedImage = {
    var bufferedImage = image.f_getFeature.getFeature
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

case class LireCEDD(val scaleWidth: Int, val scaleHeight: Int) extends IFeature with IComparable[net.semanticmetadata.lire.imageanalysis.CEDD] {
  def getFeature(image: LireImage): IDocument[net.semanticmetadata.lire.imageanalysis.CEDD] = {
    val bufferedImage = FeatureUtils.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.CEDD
    lireFeature.extract(bufferedImage)
    new IDocument[net.semanticmetadata.lire.imageanalysis.CEDD] {
      override def compareTo(other: IComparable[net.semanticmetadata.lire.imageanalysis.CEDD]) = false
    }
  }
}

case class LireFCTH(val scaleWidth: Int, val scaleHeight: Int) extends IFeature with IComparable[net.semanticmetadata.lire.imageanalysis.FCTH] {
  def getFeature(image: LireImage): IDocument[net.semanticmetadata.lire.imageanalysis.FCTH] = {
    val bufferedImage = FeatureUtils.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.FCTH
    lireFeature.extract(bufferedImage)
    new IDocument[net.semanticmetadata.lire.imageanalysis.FCTH] {
      override def compareTo(other: IComparable[net.semanticmetadata.lire.imageanalysis.FCTH]) = false
    }
  }
}

case class LireColorLayout(val scaleWidth: Int, val scaleHeight: Int) extends IFeature with IComparable[net.semanticmetadata.lire.imageanalysis.ColorLayout] {
  def getFeature(image: LireImage): IDocument[net.semanticmetadata.lire.imageanalysis.ColorLayout] = {
    val bufferedImage = FeatureUtils.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.ColorLayout
    lireFeature.extract(bufferedImage)
    new IDocument[net.semanticmetadata.lire.imageanalysis.ColorLayout] {
      override def compareTo(other: IComparable[net.semanticmetadata.lire.imageanalysis.ColorLayout]) = false
    }
  }
}

case class LireEdgeHistogram(val scaleWidth: Int, val scaleHeight: Int) extends IFeature with IComparable[net.semanticmetadata.lire.imageanalysis.EdgeHistogram] {
  def getFeature(image: LireImage): IDocument[net.semanticmetadata.lire.imageanalysis.EdgeHistogram] = {
    val bufferedImage = FeatureUtils.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.EdgeHistogram
    lireFeature.extract(bufferedImage)
    new IDocument[net.semanticmetadata.lire.imageanalysis.EdgeHistogram] {
      override def compareTo(other: IComparable[net.semanticmetadata.lire.imageanalysis.EdgeHistogram]) = false
    }
  }
}

case class LireGabor(val scaleWidth: Int, val scaleHeight: Int) extends IFeature with IComparable[net.semanticmetadata.lire.imageanalysis.Gabor] {
  def getFeature(image: LireImage): IComparable[net.semanticmetadata.lire.imageanalysis.Gabor] = {
    val bufferedImage = FeatureUtils.getFeature(image, scaleWidth, scaleHeight)
    val lireFeature = new net.semanticmetadata.lire.imageanalysis.Gabor
    lireFeature.extract(bufferedImage)
    new IDocument[net.semanticmetadata.lire.imageanalysis.Gabor] {
      override def compareTo(other: IComparable[net.semanticmetadata.lire.imageanalysis.Gabor]) = false
    }
  }
}

import net.semanticmetadata.lire.imageanalysis.sift.Feature
import net.semanticmetadata.lire.imageanalysis.sift.Extractor
case class LireSIFT() extends IFeature with IComparable[List[Feature]] {
  def getFeature(image: LireImage, scaleWidth: Int = Constants.SCALE_WIDTH, scaleHeight: Int = Constants.SCALE_HEIGHT): IComparable[List[Feature]] = {
    var features: List[Feature] = null
    try {
      var bufferedImage = image.f_getFeature.getFeature
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
    new IDocument[List[Feature]] {
      override def compareTo(other: IComparable[List[Feature]]) = false
    }
  }
}

import net.semanticmetadata.lire.utils.LuceneUtils
import org.apache.lucene.index.IndexWriter
case class LireIndex() extends IFeature with IComparable[org.apache.lucene.document.Document] {
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def getFeature(location: String, features: List[List[org.apache.lucene.document.Document]]): String = {
    val indexWriter = LuceneUtils.createIndexWriter(location, true)
    features.foreach(featureList => featureList.foreach(feature => {
      IndexUtils.addOneDocToExistingIndex(indexWriter, feature)
    }))
    indexWriter.close
    location
  }
}

case class LireQuery() {

}

  