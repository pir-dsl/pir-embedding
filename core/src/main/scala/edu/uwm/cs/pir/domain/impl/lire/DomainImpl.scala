package edu.uwm.cs.pir.domain.impl.lire

import edu.uwm.cs.pir.domain._

trait GlobalFeature extends  CEDD with Gabor with ColorLayout with FCTH 
trait LocalFeature extends SIFT
trait Training extends Clustering with LatentTopic with CCA

trait ComparableFeature { type Feature[X] = Comparable[X] }
trait StringPath { type Path = String }

trait ImageQueryFunction extends Loading with CEDD with FCTH with Indexing
trait SFAFunction extends Loading with CEDD with Gabor with ColorLayout with Similarity

trait ComparableSimilarity extends Similarity with ComparableFeature {
  type Distance[X] = Integer

  override def f_distance[X <: Comparable[X]] = (x: X) => (y: X) => { val c = x.compareTo(y); new Integer(if (c < 0) -c else c) }
  override def f_order[X <: Comparable[X]] = (x: (ID, X), y: (ID, X)) => x._2.compareTo(y._2) < 0
}

// Test implementation of feature composition 
trait ComparableComposition extends Composition with ComparableFeature {
  
  case class ComposeImpl[X <: Comparable[X], Y <: Comparable[Y]](l: X, r: Y) extends Comparable[ComposeImpl[X, Y]] {
    def compareTo(that: ComposeImpl[X, Y]) = l.compareTo(that.l) + r.compareTo(that.r)
  }

  type Compose[X <: Feature[X], Y <: Feature[Y]] = ComposeImpl[X, Y]

  def f_compose[X <: Feature[X], Y <: Feature[Y]] = (x: X, y: Y) => ComposeImpl(x, y)
}

// Test implementation of the MIR domain functions
trait LireFunction extends ImageQueryFunction with ComparableComposition {
  type ID = Int
  //type Image = LireImage
 // type Text = LireText
//  type CEDD = LireCEDD; type FCTH = LireFCTH; 
 // type SIFT = LireSIFT
  //type PATH = String
  
  //type Index[X] = String;		

 // def f_image = (p: Path) => LireImage(null, null)
//  def f_text = (p: Path) => "text" 
//  def f_cedd = (i: Image) => "CEDD"
//  def f_fcth = (i: Image) => "FCTH"
//  def f_sift = (i: Image) => "SIFT"
//  def f_index[X] = (s: List[(ID, X)]) => "Index"
//  def f_query[X] = (k: X, i: Index[X]) => List()

}
