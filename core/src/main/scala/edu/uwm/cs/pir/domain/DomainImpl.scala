package edu.uwm.cs.pir.domain

import edu.uwm.cs.pir.domain._
import net.semanticmetadata.lire.imageanalysis.LireFeature

trait WikiFeatures extends WikiFeature

trait GlobalFeatures extends AutoColorCorrelogram with BasicFeatures with BinaryPatternsPyramid with CEDD
			with ColorLayout with EdgeHistogram with FCTH with FuzzyColorHistogram with FuzzyOpponentHistogram 
			with Gabor with JCD with JpegCoefficientHistogram with LocalBinaryPatterns with LuminanceLayout 
			with OpponentHistogram with PHOG with RotationInvariantLocalBinaryPatterns with ScalableColor with SimpleColorHistogram with Tamura

trait LocalFeatures extends SIFT with SurfFeature with MSER

trait Training extends Clustering with LatentTopic with CCA

trait StringPath { type Path = String }

trait GlobalFeatureLoadFunction extends Loading with CEDD with ColorLayout with EdgeHistogram with FCTH with FuzzyColorHistogram 
      with FuzzyOpponentHistogram with Gabor with JCD with JpegCoefficientHistogram with LocalBinaryPatterns with LuminanceLayout 
      with OpponentHistogram with PHOG with RotationInvariantLocalBinaryPatterns with ScalableColor with SimpleColorHistogram with Tamura

trait LocalFeatureLoadFunction extends Loading with SIFT with SurfFeature with MSER

trait FeatureLoadFunction extends GlobalFeatureLoadFunction with LocalFeatureLoadFunction

trait ImageQueryFunction[X] extends FeatureLoadFunction with Indexing

trait SFAFunction extends Similarity 

trait SimpleComposition extends Composition {
  type Compose[X, Y] = (X, Y)
  def f_compose[X, Y] = (x: X, y: Y) => (x, y)
}

//class IntWrapper (val x : Int) extends ComparableData[IntWrapper] {
//  override def compareTo (t : IntWrapper) = x.compare(t.x)
//}
//
//class FloatWrapper (val x : Float) extends ComparableData[FloatWrapper] {
//  override def compareTo (t : FloatWrapper) = x.compare(t.x)
//}
//
//class StringWrapper (val x : String)  extends ComparableData[StringWrapper] {
//  override def compareTo (t : StringWrapper) = x.compare(t.x)
//}
//
//import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
//case class Location(val url: String, val awsS3Config: AWSS3Config)
//
//import java.awt.image.BufferedImage
//import edu.uwm.cs.pir.utils.ImageUtils
//class LireImage(val p: Location) extends Data[LireImage] {
//  def f_getFeature: BufferedImage = {
//    ImageUtils.readInputAsImage(p.url)
//  }
//}
//
//import edu.uwm.cs.pir.utils.FileUtils
//class LireText(val p: Location) extends Data[LireText] {
//  def f_getFeature: String = {
//    FileUtils.readTextFile(p.url)
//  }
//}
//
//class LireCEDDWrapper (x : net.semanticmetadata.lire.imageanalysis.CEDD) extends Data[LireCEDDWrapper] {
//}
//
//class LireFCTHWrapper (x : net.semanticmetadata.lire.imageanalysis.FCTH) extends Data[LireFCTHWrapper] {
//}
//
//import edu.uwm.cs.pir.spark.SparkObject._
//trait LireFeatureConvertFunction extends FeatureLoadFunction {
//  type Image = LireImage; type Text = LireText
//  type CEDD = LireCEDDWrapper; type FCTH = LireFCTHWrapper
//
//  awsS3Config = initAWSS3Config
//  def f_image = (p: Path) => new LireImage(new Location("some image file location", awsS3Config))
//  def f_text = (p: Path) => new LireText(new Location("some text file location", awsS3Config))
//
//  def f_cedd = (i: Image) => {
//    val cedd = new net.semanticmetadata.lire.imageanalysis.CEDD
//    cedd.extract(i.f_getFeature)
//    new LireCEDDWrapper(cedd)
//  }
//  
//  def f_fcth = (i: Image) => {
//    val fcth = new net.semanticmetadata.lire.imageanalysis.FCTH
//    fcth.extract(i.f_getFeature)
//    new LireFCTHWrapper(fcth)
//  }
//}
//
//class LireCEDDFeatureToIndexData extends FeatureToIndexData[Data[LireCEDDWrapper]] {
//    override def f_feature2Index : Data[LireCEDDWrapper] => IndexData[Data[LireCEDDWrapper]] = null
//}
