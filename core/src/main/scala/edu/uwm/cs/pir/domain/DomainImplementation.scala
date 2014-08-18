package edu.uwm.cs.pir.domain

import edu.uwm.cs.pir.domain._

trait GlobalFeature extends CEDD with Gabor with ColorLayout with FCTH
trait LocalFeature extends SIFT
trait Training extends Clustering with LatentTopic with CCA

trait StringPath { type Path = String }

trait FeatureLoadFunction extends Loading with CEDD with FCTH
trait ImageQueryFunction extends FeatureLoadFunction with Indexing
trait SFAFunction extends Loading with CEDD with Gabor with ColorLayout with Similarity

class IntWrapper (val x : Int) extends ComparableData[IntWrapper] {
  override def compareTo (t : IntWrapper) = x.compare(t.x)
}

class StringWrapper (val x : String)  extends ComparableData[StringWrapper] {
  override def compareTo (t : StringWrapper) = x.compare(t.x)
}

//trait ComparableSimilarity extends Similarity {
//  type Distance[X] = IntWrapper
//
//  class IntegerComparableData (x : Int) extends ComparableData[Int] {
//    override def compareTo(t: Int) = x.compareTo(t)
//  }
//  
//  override def f_distance[X <: Comparable[X]] = (x: X) => (y: X) => { val c = x.compareTo(y); new IntWrapper (if (c < 0) -c else c) }
//  override def f_order[X <: Comparable[X]] = (x: (ID, X), y: (ID, X)) => x._2.compareTo(y._2) < 0
//}
//
//// Test implementation of feature composition 
//
//trait ComparableComposition extends Composition {
//
//  case class ComposeImpl[X, Y](l: X, r: Y) extends Comparable[ComposeImpl[X, Y]] {
//    def compareTo(that: ComposeImpl[X, Y]) = l.compareTo(that.l) + r.compareTo(that.r)
//  }
//
//  type Compose[X, Y] = ComposeImpl[X, Y]
//
//  def f_compose[X, Y] = (x: X, y: Y) => ComposeImpl(x, y)
//}

trait SimpleSimilarity extends Similarity {
  type Distance[X] = IntWrapper
  override def f_distance[X <: Comparable[X]] = (x: X) => (y: X) => { val c = x.compareTo(y); new IntWrapper (if (c < 0) -c else c) }
  override def f_order[X <: Comparable[X]] = (x: (ID, X), y: (ID, X)) => x._2.compareTo(y._2) < 0
}

trait SimpleComposition extends Composition {
  
  case class SimpleComposeImpl[X, Y](l: X, r: Y) {
	  (l, r)
  }
    
  type Compose[X, Y] = SimpleComposeImpl[X, Y]

  def f_compose[X, Y] = (x: X, y: Y) => SimpleComposeImpl(x, y)
}

trait FeatureData { type Feature[X] = Data[X] }

import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
case class Location(val url: String, val awsS3Config: AWSS3Config)

import java.awt.image.BufferedImage
import edu.uwm.cs.pir.utils.ImageUtils
class LireImage(val p: Location) extends Data[LireImage] {
  def f_getFeature: BufferedImage = {
    ImageUtils.readInputAsImage(p.url)
  }
}

import edu.uwm.cs.pir.utils.FileUtils
class LireText(val p: Location) extends Data[LireText] {
  def f_getFeature: String = {
    FileUtils.readTextFile(p.url)
  }
}

class LireCEDDWrapper (x : net.semanticmetadata.lire.imageanalysis.CEDD) extends Data[LireCEDDWrapper] {
}

class LireFCTHWrapper (x : net.semanticmetadata.lire.imageanalysis.FCTH) extends Data[LireFCTHWrapper] {
}

trait LireFeatureConvertFunction extends FeatureLoadFunction with FeatureData {
  //type ID = Int
  type Image = LireImage; type Text = LireText
  type CEDD = LireCEDDWrapper; type FCTH = LireFCTHWrapper

  def f_image = (p: Path) => new LireImage(new Location("", null))
  def f_text = (p: Path) => new LireText(new Location("", null))

  def f_cedd = (i: Image) => {
    val cedd = new net.semanticmetadata.lire.imageanalysis.CEDD
    cedd.extract(i.f_getFeature)
    new LireCEDDWrapper(cedd)
  }
  def f_fcth = (i: Image) => {
    val fcth = new net.semanticmetadata.lire.imageanalysis.FCTH
    fcth.extract(i.f_getFeature)
    new LireFCTHWrapper(fcth)
  }
}

class LireCEDDFeatureToIndexData extends FeatureToIndexData[Data[LireCEDDWrapper]] {
    override def f_feature2Index : Data[LireCEDDWrapper] => IndexData[Data[LireCEDDWrapper]] = null
}

trait LireIndexFunction extends Indexing {
  //type ID = Int
  type Index[X] = String;		

  def f_index[X] = (s: List[(ID, X)]) => "Index"
  def f_query[X] = (k: X, i: Index[X]) => List()
}

// Test implementation of the MIR domain functions
trait StringFunction extends ImageQueryFunction {
  type ID = Int
  type Image = StringWrapper; type Text = StringWrapper
  type CEDD = StringWrapper; type FCTH = StringWrapper; type SIFT = StringWrapper

  type Index[X] = String; // type Index2[X, Y] = String;		

  def f_image = (p: Path) => new StringWrapper("image")
  def f_text = (p: Path) => new StringWrapper("text")
  def f_cedd = (i: Image) => new StringWrapper("CEDD")
  def f_fcth = (i: Image) => new StringWrapper("FCTH")
  //  def f_sift = (i: Image) => "SIFT"
  def f_index[X] = (s: List[(ID, X)]) => "Index"
  def f_query[X] = (k: X, i: Index[X]) => List()
  //  def f_index2[X, Y] = (s: List[(ID, (X, Y))]) => "Index"
  //  def f_query2[X, Y] = (k: (X, Y), i: Index2[X, Y]) => List()
}

// Test implementation of the MIR domain functions
trait NumberFunction extends Loading with GlobalFeature with LocalFeature with Indexing with Training with StringPath {

  type Image = IntWrapper; type Text = IntWrapper
  type CEDD = IntWrapper; type FCTH = IntWrapper
  type ColorLayout = IntWrapper; type Gabor = IntWrapper
  type SIFT = IntWrapper
  type Cluster[X] = IntWrapper; type Histogram[X] = X
  type Topic[X] = IntWrapper; type Distribution[X] = X
  type CCA[X, Y] = List[FeatureDoc[(X, Y)]]

  type Index[X] = List[FeatureDoc[X]]; // type Index2[X, Y] = List[(ID, (X, Y))]; 

  val l1 = "image_".length; val l2 = "text_".length
  def f_image = (p: Path) => new IntWrapper (p.substring(l1, l1 + 1).toInt)
  def f_text = (p: Path) => new IntWrapper (p.substring(l2, l2 + 1).toInt)
  def f_cedd = (i: Image) => new IntWrapper (i.x * 2)
  def f_fcth = (i: Image) => new IntWrapper (i.x * 3)

  def f_sift = (i: Image) => new IntWrapper (i.x * 4)

  def f_colorlayout = (i: Image) => new IntWrapper (i.x * 1)
  def f_gabor = (i: Image) => new IntWrapper (i.x * 3)

  def f_cluster_train[X] = (s: List[(ID, X)]) => new IntWrapper (1)
  def f_cluster_proj[X] = (x: X, c: Cluster[X]) => x

  def f_lda_train[X] = (s: List[(ID, X)]) => new IntWrapper (1)
  def f_lda_proj[X] = (x: X, t: Topic[X]) => x

  def f_cca_train[X, Y] = (l: List[(ID, (X, Y))]) => l
  def f_cca_proj1[X, Y] = (x: X, cca: CCA[X, Y]) => cca.filter(t => t._2 == x).map(t => t._1)
  def f_cca_proj2[X, Y] = (y: Y, cca: CCA[X, Y]) => cca.filter(t => t._2 == y).map(t => t._1)

  def f_index[X] = (s: List[(ID, X)]) => s
  def f_query[X] = (k: X, i: Index[X]) => i.filter(p => (p._2 == k)).map(p => p._1)

  //  def f_index2[X, Y] = (s: List[(ID, (X, Y))]) => s
  //  def f_query2[X, Y] = (k: (X, Y), i: Index2[X, Y]) =>
  //    i.filter(p => p._2._1.compareTo(k._1) == 0 && p._2._2.compareTo(k._2) == 0)
  //    .map(p => p._1)
}
