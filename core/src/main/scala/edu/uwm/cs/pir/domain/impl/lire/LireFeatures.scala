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

trait LireDomain extends Domain {
  type Image = edu.uwm.cs.mir.prototypes.feature.Image
  type Text = edu.uwm.cs.mir.prototypes.feature.Text
}

//TODO
trait LireLuceneGlobal extends Lucene {
  type Feature = LireFeature
  type Document = org.apache.lucene.document.Document
  def f_luceneDocTransformer = (feature : Feature) => {
    var doc = new org.apache.lucene.document.Document
    //doc = IndexUtils.getLuceneDocument(lireFeatureAdaptor.getId(), lireFeatureAdaptor.getLireFeature(), type);   
    doc
  }
}


//TODO
import net.semanticmetadata.lire.utils.LuceneUtils
import org.apache.lucene.index.IndexWriter
trait LireIndexFunction extends Indexing {
  var indexLocation : String
  type Index[X] = edu.uwm.cs.mir.prototypes.index.LuceneIndex

  def f_index[LireFeature] = (s: List[(ID, LireFeature)]) => {

	var indexWriter : IndexWriter = null;
	try {
	    indexWriter = LuceneUtils.createIndexWriter(indexLocation, true);
	} catch {
	  case e : java.io.IOException => throw new RuntimeException(e);
	}

	s.foreach {
	   // for (IFeature f : fs) {
	  
	  elem => elem._1
	  
//		if (f instanceof LuceneFeatureAdaptor) {
//		    LuceneFeatureAdaptor luceneFeatureAdaptor = (LuceneFeatureAdaptor) f;
//		    Document doc = luceneFeatureAdaptor.getDoc();
//		    try {
//			if (doc != null) {
//			    IndexUtils.addOneDocToExistingIndex(indexWriter, doc);
//			}
//		    } catch (IOException e) {
//			// We may want to allow some failed index operations and
//			// continue with the process with other documents later
//			// so need to revisit this
//			throw new RuntimeException(e);
//		    }
//		}
	    //}
	}
	try {
	    indexWriter.close();
	} catch {
	  case e : Exception => throw new RuntimeException(e);
	}
	new edu.uwm.cs.mir.prototypes.index.LuceneIndex(indexLocation);
    
    
    
    
    
    
    
    
    
  }
  def f_query[X] = (k: X, i: Index[X]) => List()
}

trait LireTraining extends Training

trait LireGlobalFeatures extends GlobalFeatures with LireDomain {
  var scaleWidth: Int;var scaleHeight: Int
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

trait LireLocalFeatures extends LocalFeatures