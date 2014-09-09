package edu.uwm.cs.pir.domain.impl.lire

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.GlobalFeatures
import edu.uwm.cs.pir.domain.LocalFeatures
import edu.uwm.cs.pir.domain.Indexing
import edu.uwm.cs.pir.domain.Training
import edu.uwm.cs.pir.domain.Lucene
import java.awt.image.BufferedImage
import edu.uwm.cs.mir.prototypes.feature.lire.utils.LireFeatureUtils
import net.semanticmetadata.lire.imageanalysis.LireFeature
import edu.uwm.cs.mir.prototypes.feature.utils.FeatureUtils
import scala.collection.JavaConverters._

trait LireDomain extends Domain {
  type Image = edu.uwm.cs.mir.prototypes.feature.Image
  type Text = edu.uwm.cs.mir.prototypes.feature.Text
}

//TODO
//trait LireLuceneGlobal extends Lucene {
//  type Feature = LireFeature
//  type Document = org.apache.lucene.document.Document
//  def f_luceneDocTransformer = (feature: Feature) => {
//    var doc = new org.apache.lucene.document.Document
//    doc
//  }
//}

import net.semanticmetadata.lire.utils.LuceneUtils
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import net.semanticmetadata.lire.DocumentBuilder
import edu.uwm.cs.mir.prototypes.query.utils.QueryUtils
import net.semanticmetadata.lire.ImageSearchHits
import net.semanticmetadata.lire.impl.SimpleImageSearchHits
import net.semanticmetadata.lire.impl.SimpleResult
import java.util.TreeSet
import scala.collection.immutable.HashMap

trait LireIndexFunction[X] extends Indexing {
  var indexLocation: String
  type Index[X] = edu.uwm.cs.mir.prototypes.index.LuceneIndex

  def f_index[X] = (s: List[(ID, X)]) => {
    var indexWriter: IndexWriter = null;
    try {
      indexWriter = LuceneUtils.createIndexWriter(indexLocation, true);
    } catch {
      case e: java.io.IOException => throw new RuntimeException(e);
    }

    s.foreach {
      elem =>
        {
          val id = elem._1
          val lireFeature = elem._2
          val doc = new Document
          if (lireFeature == null) {
            doc.add(new Field(lireFeature.getClass.getSimpleName(), Array[Byte]()));
          } else {
            //TODO: check to see if we can avoid the below
            lireFeature match {
              case (x, y) => {
                doc.add(new Field(x.getClass.getSimpleName(), x.asInstanceOf[LireFeature].getByteArrayRepresentation))
                doc.add(new Field(y.getClass.getSimpleName(), y.asInstanceOf[LireFeature].getByteArrayRepresentation))
              }
              case _ => doc.add(new Field(lireFeature.getClass.getSimpleName(), lireFeature.asInstanceOf[LireFeature].getByteArrayRepresentation))
            }
          }
          if (id != null) doc.add(new Field(DocumentBuilder.FIELD_NAME_IDENTIFIER, id.toString, Field.Store.YES, Field.Index.NOT_ANALYZED));
          try {
            if (doc != null) {
              indexWriter.addDocument(doc);
            }
          } catch {
            // We may want to allow some failed index operations and continue with the process with 
            // other documents later so need to revisit this
            case e: java.io.IOException => throw new RuntimeException(e);
          }
        }
    }

    try {
      indexWriter.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
    new edu.uwm.cs.mir.prototypes.index.LuceneIndex(indexLocation);

  }

  def f_query[X] = (k: X, i: Index[X]) => {
    //TODO: check to see if we can avoid the below
    val lireSearchResult = {
      k match {
        case (x, y) => {
          var queryFeature = x.asInstanceOf[LireFeature]
          val searchHitsX = queryIndex(queryFeature)
          queryFeature = y.asInstanceOf[LireFeature]
          val searchHitsY = queryIndex(queryFeature)
          combineFeatureResult(searchHitsX, searchHitsY)
        }
        case _ => {
          val queryFeature = k.asInstanceOf[LireFeature]
          convertFeatureResult(queryIndex(queryFeature))
        }
      }
    }

    val resultArray = lireSearchResult.toArray(Array[LireSearchResult]())
    resultArray.foreach(println(_))
    val resultLength = lireSearchResult.size
    var resultList = List[ID]()
    for (i: Int <- 0 to resultLength - 1) {
      resultList = resultList.:: {
        val id = resultArray(i).id
        id /*.substring(id.lastIndexOf('/') + 1, id.length)*/ .asInstanceOf[ID]
      }
    }
    resultList
  }

  case class LireSearchResult(val score: Float, val id: String) extends java.lang.Comparable[LireSearchResult] {
    override def toString() = {
      "id = " + id + ", score = " + score
    }

    //Descending order
    override def compareTo(that: LireSearchResult) = {
      if (this.score < that.score) 1
      else if (this.score > that.score) -1
      else 0
    }
  }

  private def convertFeatureResult(searchHits: ImageSearchHits): TreeSet[LireSearchResult] = {
    val docs: TreeSet[LireSearchResult] = new TreeSet[LireSearchResult]();
    var maxDistance = -1f;
    for (i: Int <- 0 to searchHits.length - 1) {
      val tmpDistance = searchHits.score(i)
      if (maxDistance < 0) {
        maxDistance = tmpDistance;
      }
      docs.add(new LireSearchResult(tmpDistance, searchHits.doc(i).getField(DocumentBuilder.FIELD_NAME_IDENTIFIER).stringValue));
      if (tmpDistance > maxDistance) maxDistance = tmpDistance;
    }
    docs
  }

  private def combineFeatureResult(searchHitsX: ImageSearchHits, searchHitsY: ImageSearchHits, weightPercentage: Float = 0.5F): TreeSet[LireSearchResult] = {
    if (searchHitsX.length != searchHitsY.length) throw new Exception("Results from two different feature queries are not in the same size!")
    else {

      var xMap: Map[String, Float] = Map()
      var yMap: Map[String, Float] = Map()

      for (i: Int <- 0 to searchHitsX.length - 1) {
        xMap += (searchHitsX.doc(i).getField(DocumentBuilder.FIELD_NAME_IDENTIFIER).stringValue -> searchHitsX.score(i))
        yMap += (searchHitsY.doc(i).getField(DocumentBuilder.FIELD_NAME_IDENTIFIER).stringValue -> searchHitsY.score(i))
      }

      val docs: TreeSet[LireSearchResult] = new TreeSet[LireSearchResult]();
      var maxDistance = -1f;

      xMap.keySet.foreach(key => {
        val score1 = xMap.get(key).get
        val score2 = yMap.get(key).get
        val tmpDistance = (xMap.get(key).get * weightPercentage + yMap.get(key).get * (1 - weightPercentage))
        if (maxDistance < 0) {
          maxDistance = tmpDistance;
        }
        docs.add(new LireSearchResult(tmpDistance, key));
        if (tmpDistance > maxDistance) maxDistance = tmpDistance;
      })
      docs
    }
  }

  private def queryIndex(queryFeature: net.semanticmetadata.lire.imageanalysis.LireFeature): ImageSearchHits = {
    val featureType = queryFeature.getFeatureName.filter(char => if (' ' == char) false else true)

    val queryDoc = new Document
    if (queryFeature == null) {
      queryDoc.add(new Field(queryFeature.getClass.getSimpleName(), Array[Byte]()));
    } else {
      //TODO: check to see if we can avoid the below
      queryDoc.add(new Field(queryFeature.getClass.getSimpleName(), queryFeature.asInstanceOf[LireFeature].getByteArrayRepresentation))
    }

    var imageSearchHits: ImageSearchHits = null
    try {
      imageSearchHits = QueryUtils.getLuceneImageSearchHtsList(queryDoc, featureType, indexLocation);
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
    imageSearchHits
  }
}

import edu.uwm.cs.pir.domain.Similarity
trait LireGlobalSimilarity extends Similarity {
  type X = LireFeature
  type Distance = Float
  def f_distance(y: X): PrjOp[X, Distance] = {
    x =>
      {
        x.getDistance(y)
      }
  }
  def f_order: OdrOp[Distance] = (x, y) => {
    if (x._2 - y._2 > 0)
      true
    else
      false
  }
}

trait LireTraining extends Training

trait LireGlobalFeatures extends GlobalFeatures with LireDomain {
  var scaleWidth: Int; var scaleHeight: Int
  type AutoColorCorrelogram = net.semanticmetadata.lire.imageanalysis.AutoColorCorrelogram
  type BasicFeatures = net.semanticmetadata.lire.imageanalysis.BasicFeatures
  type BinaryPatternsPyramid = net.semanticmetadata.lire.imageanalysis.BinaryPatternsPyramid
  type CEDD = net.semanticmetadata.lire.imageanalysis.CEDD
  type ColorLayout = net.semanticmetadata.lire.imageanalysis.ColorLayout
  type EdgeHistogram = net.semanticmetadata.lire.imageanalysis.EdgeHistogram
  type FCTH = net.semanticmetadata.lire.imageanalysis.FCTH
  type FuzzyColorHistogram = net.semanticmetadata.lire.imageanalysis.FuzzyColorHistogram
  type FuzzyOpponentHistogram = net.semanticmetadata.lire.imageanalysis.FuzzyOpponentHistogram
  type Gabor = net.semanticmetadata.lire.imageanalysis.Gabor
  type JCD = net.semanticmetadata.lire.imageanalysis.JCD
  type JpegCoefficientHistogram = net.semanticmetadata.lire.imageanalysis.JpegCoefficientHistogram
  type LocalBinaryPatterns = net.semanticmetadata.lire.imageanalysis.LocalBinaryPatterns
  type LuminanceLayout = net.semanticmetadata.lire.imageanalysis.LuminanceLayout
  type OpponentHistogram = net.semanticmetadata.lire.imageanalysis.OpponentHistogram
  type PHOG = net.semanticmetadata.lire.imageanalysis.PHOG
  type RotationInvariantLocalBinaryPatterns = net.semanticmetadata.lire.imageanalysis.RotationInvariantLocalBinaryPatterns
  type ScalableColor = net.semanticmetadata.lire.imageanalysis.ScalableColor
  type SimpleColorHistogram = net.semanticmetadata.lire.imageanalysis.SimpleColorHistogram
  type Tamura = net.semanticmetadata.lire.imageanalysis.Tamura

  def f_autoColorCorrelogram: PrjOp[Image, AutoColorCorrelogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new AutoColorCorrelogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.AutoColorCorrelogram]
  }

  def f_basicFeatures: PrjOp[Image, BasicFeatures] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new BasicFeatures).asInstanceOf[net.semanticmetadata.lire.imageanalysis.BasicFeatures]
  }

  def f_binaryPatternsPyramid: PrjOp[Image, BinaryPatternsPyramid] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new BinaryPatternsPyramid).asInstanceOf[net.semanticmetadata.lire.imageanalysis.BinaryPatternsPyramid]
  }

  def f_cedd: PrjOp[Image, CEDD] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new CEDD).asInstanceOf[net.semanticmetadata.lire.imageanalysis.CEDD]
  }

  def f_colorLayout: PrjOp[Image, ColorLayout] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new ColorLayout).asInstanceOf[net.semanticmetadata.lire.imageanalysis.ColorLayout]
  }

  def f_edgeHistogram: PrjOp[Image, EdgeHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new EdgeHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.EdgeHistogram]
  }

  def f_fcth: PrjOp[Image, FCTH] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new FCTH).asInstanceOf[net.semanticmetadata.lire.imageanalysis.FCTH]
  }

  def f_fuzzyColorHistogram: PrjOp[Image, FuzzyColorHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new FuzzyColorHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.FuzzyColorHistogram]
  }

  def f_fuzzyOpponentHistogram: PrjOp[Image, FuzzyOpponentHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new FuzzyOpponentHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.FuzzyOpponentHistogram]
  }

  def f_gabor: PrjOp[Image, Gabor] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new Gabor).asInstanceOf[net.semanticmetadata.lire.imageanalysis.Gabor]
  }

  def f_jcd: PrjOp[Image, JCD] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new JCD).asInstanceOf[net.semanticmetadata.lire.imageanalysis.JCD]
  }

  def f_jpegCoefficientHistogram: PrjOp[Image, JpegCoefficientHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new JpegCoefficientHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.JpegCoefficientHistogram]
  }

  def f_localBinaryPatterns: PrjOp[Image, LocalBinaryPatterns] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new LocalBinaryPatterns).asInstanceOf[net.semanticmetadata.lire.imageanalysis.LocalBinaryPatterns]
  }

  def f_luminanceLayout: PrjOp[Image, LuminanceLayout] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new LuminanceLayout).asInstanceOf[net.semanticmetadata.lire.imageanalysis.LuminanceLayout]
  }

  def f_opponentHistogram: PrjOp[Image, OpponentHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new OpponentHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.OpponentHistogram]
  }

  def f_phog: PrjOp[Image, PHOG] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new PHOG).asInstanceOf[net.semanticmetadata.lire.imageanalysis.PHOG]
  }

  def f_rotationInvariantLocalBinaryPatterns: PrjOp[Image, RotationInvariantLocalBinaryPatterns] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new RotationInvariantLocalBinaryPatterns).asInstanceOf[net.semanticmetadata.lire.imageanalysis.RotationInvariantLocalBinaryPatterns]
  }

  def f_scalableColor: PrjOp[Image, ScalableColor] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new ScalableColor).asInstanceOf[net.semanticmetadata.lire.imageanalysis.ScalableColor]
  }

  def f_simpleColorHistogram: PrjOp[Image, SimpleColorHistogram] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new SimpleColorHistogram).asInstanceOf[net.semanticmetadata.lire.imageanalysis.SimpleColorHistogram]
  }

  def f_tamura: PrjOp[Image, Tamura] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      processLireFeature(image, new Tamura).asInstanceOf[net.semanticmetadata.lire.imageanalysis.Tamura]
  }

  private def processLireFeature(image: edu.uwm.cs.mir.prototypes.feature.Image, lireFeature: net.semanticmetadata.lire.imageanalysis.LireFeature): net.semanticmetadata.lire.imageanalysis.LireFeature = {
    try {
      var bufferedImage = image.getFeature;
      // When this is null, the jpg file cannot be processed
      if (bufferedImage != null) {
        val imageType = if (bufferedImage.getType == 0) BufferedImage.TYPE_INT_ARGB else bufferedImage.getType
        bufferedImage = LireFeatureUtils.resizeImageWithHint(bufferedImage, imageType, scaleWidth, scaleHeight);
        lireFeature.extract(bufferedImage);
      }
    } catch {
      case e: Exception => println(e.getMessage());
    }
    lireFeature
  }
}

import net.semanticmetadata.lire.imageanalysis.mser.MSER
import net.semanticmetadata.lire.utils.ImageUtils

trait LireLocalFeatures extends LocalFeatures with LireDomain {
  type SIFT = List[net.semanticmetadata.lire.imageanalysis.sift.Feature]
  type SurfFeature = List[com.stromberglabs.jopensurf.SURFInterestPoint]
  type MSER = List[net.semanticmetadata.lire.imageanalysis.mser.MSERFeature]

  override def f_sift: PrjOp[Image, SIFT] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      {
        val extractor = new net.semanticmetadata.lire.imageanalysis.sift.Extractor
        extractor.computeSiftFeatures(image.getFeature).asScala.toList
      }
  }

  def f_surfFeature: PrjOp[Image, SurfFeature] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      val s = new com.stromberglabs.jopensurf.Surf(image.getFeature);
      s.getFreeOrientedInterestPoints.asScala.toList
  }

  def f_mser: PrjOp[Image, MSER] = {
    (image: edu.uwm.cs.mir.prototypes.feature.Image) =>
      {
        val extractor = new net.semanticmetadata.lire.imageanalysis.mser.MSER
        val greyImage: BufferedImage = convertImageToGrey(image.getFeature)
        val features = extractor.computeMSERFeatures(greyImage)
        ImageUtils.invertImage(greyImage);
        features.addAll(extractor.computeMSERFeatures(greyImage))
        features.asScala.toList
      }
  }

  private def convertImageToGrey(image: BufferedImage): BufferedImage = {
    val result = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    result.getGraphics.drawImage(image, 0, 0, null)
    result
  }
}