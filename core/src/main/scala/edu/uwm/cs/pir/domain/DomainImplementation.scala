package edu.uwm.cs.pir.domain

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
trait StringFunction extends ImageQueryFunction with ComparableComposition {
  type ID = Int
  type Image = String; type Text = String
  type CEDD = String; type FCTH = String; type SIFT = String

  type Index[X] = String; // type Index2[X, Y] = String;		

  def f_image = (p: Path) => "image" 
  def f_text = (p: Path) => "text" 
  def f_cedd = (i: Image) => "CEDD"
  def f_fcth = (i: Image) => "FCTH"
  //  def f_sift = (i: Image) => "SIFT"
  def f_index[X] = (s: List[(ID, X)]) => "Index"
  def f_query[X] = (k: X, i: Index[X]) => List()
  //  def f_index2[X, Y] = (s: List[(ID, (X, Y))]) => "Index"
  //  def f_query2[X, Y] = (k: (X, Y), i: Index2[X, Y]) => List()
}

// Test implementation of the MIR domain functions

trait NumberFunction extends Loading with GlobalFeature with LocalFeature with Indexing with Training with ComparableFeature with StringPath {

  type Image = Integer; type Text = Integer
  type CEDD = Integer; type FCTH = Integer
  type ColorLayout = Integer; type Gabor = Integer
  type SIFT = Integer
  type Cluster[X] = Integer; type Histogram[X<:Feature[X]] = X
  type Topic[X] = Integer; type Distribution[X<:Feature[X]] = X
  type CCA[X, Y] = List[FeatureDoc[(X,Y)]]

  type Index[X] = List[FeatureDoc[X]]; // type Index2[X, Y] = List[(ID, (X, Y))]; 

  
  val l1 = "image_".length; val l2 = "text_".length
  def f_image = (p: Path) =>  p.substring(l1, l1 + 1).toInt 
  def f_text = (p: Path) => p.substring(l2, l2 + 1).toInt   
  def f_cedd = (i: Image) => i * 2
  def f_fcth = (i: Image) => i * 3

  def f_sift = (i: Image) => i * 4

  def f_colorlayout = (i: Image) => i * 1
  def f_gabor = (i: Image) => i * 3

  def f_cluster_train[X<:Feature[X]] = (s: List[(ID, X)]) => 1
  def f_cluster_proj[X<:Feature[X]] = (x: X, c: Cluster[X]) => x

  def f_lda_train[X<:Feature[X]] = (s: List[(ID, X)]) => 1
  def f_lda_proj[X<:Feature[X]] = (x: X, t: Topic[X]) => x

  def f_cca_train[X<:Feature[X], Y<:Feature[Y]] = (l: List[(ID, (X, Y))]) => l
  def f_cca_proj1[X<:Feature[X], Y<:Feature[Y]] = (x: X, cca: CCA[X, Y]) => cca.filter((t) => x.compareTo(t._2._1) == 0).map(t=>t._1)
  def f_cca_proj2[X<:Feature[X], Y<:Feature[Y]] = (y: Y, cca: CCA[X, Y]) => cca.filter((t) => y.compareTo(t._2._2) == 0).map(t=>t._1)

  def f_index[X <: Feature[X]] = (s: List[(ID, X)]) => s
  def f_query[X <: Feature[X]] = (k: X, i: Index[X]) => i.filter(p => p._2.compareTo(k) == 0).map(p => p._1)

  //  def f_index2[X <: Feature[X], Y <: Feature[Y]] = (s: List[(ID, (X, Y))]) => s
  //  def f_query2[X <: Feature[X], Y <: Feature[Y]] = (k: (X, Y), i: Index2[X, Y]) =>
  //    i.filter(p => p._2._1.compareTo(k._1) == 0 && p._2._2.compareTo(k._2) == 0)
  //    .map(p => p._1)
}
