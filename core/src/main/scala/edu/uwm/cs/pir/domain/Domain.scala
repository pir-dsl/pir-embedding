package edu.uwm.cs.pir.domain

import edu.uwm.cs.pir.Base

  // MIR domain types

  trait Domain extends Base {
    type Image// <: Feature[Image] // media types
    type Text// <: Feature[Text]
  }

  // MIR domain functions

  trait Similarity extends Domain {
    type Distance[X] // point feature types
    def f_distance[X <: Comparable[X]]: X => PrjOp[X, Distance[X]] // (X, X) => Distance[X]
    def f_order[X <: Comparable[X]]: OdrOp[X] // (X, X) => Boolean
  }

  trait Composition extends Domain {
    type Compose[X, Y]
    def f_compose[X <: Comparable[X], Y <: Comparable[Y]]: CpsOp[X, Y, Compose[X, Y]] // (X, Y) => Compose[X, Y]
  }

  trait Loading extends Domain {
    type Path
    def f_image: LoadOp[Path, Image] // Path => List[Feature[Image]]
    def f_text: LoadOp[Path, Text] // Path => List[Feature[Text]]
  }

  trait Data[X]
  
  trait ComparableData[X] extends Data[X] with Comparable[X]
  
  trait IndexData[X] extends Data[X]
  
  trait RawData[X] extends Data[X]
  
  trait FeatureToIndexData[X] {
	 def f_feature2Index : X => IndexData[X]
  }
  
  trait IndexDataToFeature[X] {
    def f_index2Feature: IndexData[X] => X
  }
  
  trait Search[X] {
    def search(q : IndexData[X]) : List[IndexData[X]] 
  }
  
  // global features

  trait CEDD extends Domain {
    type CEDD
    def f_cedd: PrjOp[Image, CEDD] // Image => CEDD
  }
  
  trait FCTH extends Domain {
    type FCTH
    def f_fcth: PrjOp[Image, FCTH] // Image => FCTH
  }
  
  trait Gabor extends Domain {
    type Gabor
    def f_gabor: PrjOp[Image, Gabor] // Image => Gabor
  }
  
  trait ColorLayout extends Domain {
    type ColorLayout// <: Feature[ColorLayout]
    def f_colorlayout: PrjOp[Image, ColorLayout] // Image => ColorLayout
  }

  // local features

  trait SIFT extends Domain {
    type SIFT
    def f_sift: PrjOp[Image, SIFT] // Image => SIFT
  }

  // indexing

  trait Indexing extends Domain {
    type Index[X];
    def f_index[X]: IdxOp[X, Index[X]] // List[(ID, X)] => Index[X]
    def f_query[X]: DPrjOp[X, List[ID], Index[X]] // (X, Index[X]) => List[ID]
  }

  // not strictly needed because of feature composition
  trait Indexing2 extends Domain {
    type Index2[X, Y]
    def f_index2[X, Y]: IdxOp[(X, Y), Index2[X, Y]] // (List[(ID, (X, Y))]) => Index2[X, Y]
    def f_query2[X, Y]: DPrjOp[(X, Y), List[ID], Index2[X, Y]] // ((X, Y), Index2[X, Y]) => List[ID]
  }

  // machine learning

  trait Clustering extends Domain {
    type Histogram[X]
    type Cluster[X]

    def f_cluster_train[X]: TrnOp[X, Cluster[X]] // List[(ID, X)] => Cluster[X]
    def f_cluster_proj[X]: DPrjOp[X, Histogram[X], Cluster[X]] // (X, Cluster[X]) => Histogram[X]  
  }

  trait LatentTopic extends Domain {
    type Distribution[X]
    type Topic[X]

    def f_lda_train[X]: TrnOp[X, Topic[X]] // List[(ID, X)] => Topic[X]
    def f_lda_proj[X]: DPrjOp[X, Distribution[X], Topic[X]] // (X, Topic[X]) => Distribution[X]  
  }

  trait CCA extends Domain {
    type CCA[X, Y]

    def f_cca_train[X, Y]: TrnOp[(X, Y), CCA[X, Y]] // (List[(ID, (X, Y))]) => CCA[X, Y]
    def f_cca_proj1[X, Y]: DPrjOp[X, List[ID], CCA[X, Y]] // (X, CCA[X, Y]) => List[ID]
    def f_cca_proj2[X, Y]: DPrjOp[Y, List[ID], CCA[X, Y]] // (Y, CCA[X, Y]) => List[ID]
  }




