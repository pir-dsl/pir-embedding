package edu.uwm.cs.pir.domain

import edu.uwm.cs.pir.Base

  /* MIR domain types */
  trait Domain extends Base {
    type Image// <: Feature[Image] // media types
    type Text// <: Feature[Text]
  }

  /* MIR domain functions */
  trait Similarity extends Domain {
    type X
    type Distance // point feature types
    def f_distance : X => PrjOp[X, Distance] // (X, X) => Distance[X]
    def f_order: OdrOp[Distance] // (X, X) => Boolean
  }

  trait Composition extends Domain {
    type Compose[X, Y]
    def f_compose[X, Y]: CpsOp[X, Y, Compose[X, Y]] // (X, Y) => Compose[X, Y]
  }

  trait Loading extends Domain {
    type Path
    def f_image: LoadOp[Path, Image] // Path => List[Feature[Image]]
    def f_text: LoadOp[Path, Text] // Path => List[Feature[Text]]
  }
  
  trait WikiFeature extends Domain {
    type WikiFeatureExtractor
    def f_wikiFeatureExtractor : PrjOp[Text, WikiFeatureExtractor]
  }

  /* global features */
  //Color Correlogram is an image of correlation statistics for color
  trait AutoColorCorrelogram extends Domain {
    type AutoColorCorrelogram
    def f_autoColorCorrelogram: PrjOp[Image, AutoColorCorrelogram] // Image => AutoColorCorrelogram
  }
  
  //Contains basic image features like contrast, overall sharpness etc.
  trait BasicFeatures extends Domain {
    type BasicFeatures
    def f_basicFeatures: PrjOp[Image, BasicFeatures] // Image => BasicFeatures
  }

  //BinaryPatternsPyramid is built the same way as PHOG, but instead of measuring 
  //the orientation of gradients, this class uses a histogram of rotation-invariant local binary patterns.
  trait BinaryPatternsPyramid extends Domain {
    type BinaryPatternsPyramid
    def f_binaryPatternsPyramid: PrjOp[Image, BinaryPatternsPyramid] // Image => BinaryPatternsPyramid
  }
  
  //Color and Edge Directivity Descriptor
  trait CEDD extends Domain {
    type CEDD
    def f_cedd: PrjOp[Image, CEDD] // Image => CEDD
  }
  
  //A Color Layout Descriptor (CLD) is designed to capture the spatial distribution of color in an image
  trait ColorLayout extends Domain {
    type ColorLayout// <: Feature[ColorLayout]
    def f_colorLayout: PrjOp[Image, ColorLayout] // Image => ColorLayout
  }
  
  //Edge Histogram Descriptor
  trait EdgeHistogram extends Domain {
    type EdgeHistogram
    def f_edgeHistogram: PrjOp[Image, EdgeHistogram] // Image => EdgeHistogram
  }

  //Fuzzy Color and Texture Histogram Descriptor
  trait FCTH extends Domain {
    type FCTH
    def f_fcth: PrjOp[Image, FCTH] // Image => FCTH
  }
  
  //Fuzzy Color Histogram Descriptor
  trait FuzzyColorHistogram extends Domain {
    type FuzzyColorHistogram
    def f_fuzzyColorHistogram: PrjOp[Image, FuzzyColorHistogram] // Image => FuzzyColorHistogram
  } 
  
  //Fuzzy Opponent (color space) Descriptor
  trait FuzzyOpponentHistogram extends Domain {
    type FuzzyOpponentHistogram
    def f_fuzzyOpponentHistogram: PrjOp[Image, FuzzyOpponentHistogram] // Image => FuzzyOpponentHistogram
  }  
  
  //Gabor filter, named after Dennis Gabor, is a linear filter used for edge detection
  trait Gabor extends Domain {
    type Gabor
    def f_gabor: PrjOp[Image, Gabor] // Image => Gabor
  }
  
  //A Joint Descriptor joining CEDD and FCTH in one histogram
  trait JCD extends Domain {
    type JCD
    def f_jcd: PrjOp[Image, JCD] // Image => JCD
  }
  
  //Jpeg Coefficient Histogram Descriptor
  trait JpegCoefficientHistogram extends Domain {
    type JpegCoefficientHistogram
    def f_jpegCoefficientHistogram: PrjOp[Image, JpegCoefficientHistogram] // Image => JpegCoefficientHistogram
  }
  
  //Local Binary Pattern Texture Descriptor
  trait LocalBinaryPatterns extends Domain {
    type LocalBinaryPatterns
    def f_localBinaryPatterns: PrjOp[Image, LocalBinaryPatterns] // Image => LocalBinaryPatterns
  }
  
  //Luminance Layout Descriptor
  trait LuminanceLayout extends Domain {
    type LuminanceLayout
    def f_luminanceLayout: PrjOp[Image, LuminanceLayout] // Image => LuminanceLayout
  }
  
  //Opponent Histogram Descriptor
  trait OpponentHistogram extends Domain {
    type OpponentHistogram
    def f_opponentHistogram: PrjOp[Image, OpponentHistogram] // Image => OpponentHistogram
  }
  
  //Pyramid Histogram of Oriented Gradients Descriptor
  trait PHOG extends Domain {
    type PHOG
    def f_phog: PrjOp[Image, PHOG] // Image => PHOG
  }
  
  //Rotation Invariant Local Binary Pattern Descriptor
  trait RotationInvariantLocalBinaryPatterns extends Domain {
    type RotationInvariantLocalBinaryPatterns
    def f_rotationInvariantLocalBinaryPatterns: PrjOp[Image, RotationInvariantLocalBinaryPatterns] // Image => RotationInvariantLocalBinaryPatterns
  }
  
  //Scalable Color Descriptor
  trait ScalableColor extends Domain {
    type ScalableColor
    def f_scalableColor: PrjOp[Image, ScalableColor] // Image => ScalableColor
  }
  
  //Simple Color Histogram Descriptor
  trait SimpleColorHistogram extends Domain {
    type SimpleColorHistogram
    def f_simpleColorHistogram: PrjOp[Image, SimpleColorHistogram] // Image => SimpleColorHistogram
  }
  
  //Tamura Texture Descriptor
  trait Tamura extends Domain {
    type Tamura
    def f_tamura: PrjOp[Image, Tamura] // Image => Tamura
  }
  
  /* local features */

  //Scale-Invariant Feature Transform Descriptor
  trait SIFT extends Domain {
    type SIFT
    def f_sift: PrjOp[Image, SIFT] // Image => SIFT
  }
  
  //Speeded Up Robust Features Descriptor
  trait SurfFeature extends Domain {
    type SurfFeature
    def f_surfFeature: PrjOp[Image, SurfFeature] // Image => SurfFeature
  }
  
  //Maximally Stable Extremal Regions Descriptor
  trait MSER extends Domain {
    type MSER
    def f_mser: PrjOp[Image, MSER] // Image => MSER
  } 

  trait Lucene extends Base {
    type Feature
    type Document
    def f_luceneDocTransformer : PrjOp[Feature, Document]
  }
  
  // indexing
  trait Indexing extends Domain {
    type Index[X]
    def f_index[X]: IdxOp[X, Index[X]] // List[(ID, X)] => Index[X]
    def f_query[X]: DPrjOp[X, List[ID], Index[X]] // (X, Index[X]) => List[ID]
  }

  // not strictly needed because of feature composition
  trait Indexing2 extends Domain {
    type Index2
    def f_index2[X, Y]: IdxOp[(X, Y), Index2] // (List[(ID, (X, Y))]) => Index2
    def f_query2[X, Y]: DPrjOp[(X, Y), List[ID], Index2] // ((X, Y), Index2) => List[ID]
  }

  // machine learning
  trait Clustering extends Domain {
    type X
    type Histogram
    type Cluster

    def f_cluster_train: TrnOp[X, Cluster] // List[(ID, X)] => Cluster
    def f_cluster_proj: DPrjOp[X, Histogram, Cluster] // (X, Cluster) => Histogram[X]  
  }

  trait LatentTopic extends Domain {
    type Distribution
    type Topic
    type Lda_Input
    def f_lda_train: TrnOp[Lda_Input, Topic] // List[(ID, X)] => Topic
    def f_lda_proj: DPrjOp[Lda_Input, Distribution, Topic] // (X, Topic) => Distribution 
  }

  trait CCA extends Domain {
    type CCA_Input_X
    type CCA_Input_Y
    type CCA
    type CCAResult
    
    def f_cca_train: TrnOp[(CCA_Input_X, CCA_Input_Y), CCA] // (List[(ID, (X, Y))]) => CCA and ID 1-1 maps to ID2
    def f_cca_proj1: DPrjOp[CCA_Input_X, List[(ID, CCAResult)], CCA] // (X, CCA) => List[ID]
    def f_cca_proj2: DPrjOp[CCA_Input_Y, List[(ID, CCAResult)], CCA] // (Y, CCA) => List[ID]
  }
  
//  trait Data[X]
//  
//  trait ComparableData[X] extends Data[X] with Comparable[X]
//  
//  trait IndexData[X] extends Data[X]
//  
//  trait IndexResult[X] extends Data[X]
//  
//  trait FeatureToIndexData[X] {
//	 def f_feature2Index : X => IndexData[X]
//  }
//  
//  trait IndexDataToFeature[X] {
//    def f_index2Feature: IndexData[X] => X
//  }
//  
//  trait Search[X] {
//    def search(q : X) : List[IndexResult[X]] 
//  }
