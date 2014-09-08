package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.Constants._
import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.pipeline.Parallel
import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.Constants.SAMPLE_IMAGES_ROOT
import edu.uwm.cs.pir.utils.FileUtils

import edu.uwm.cs.pir.domain.impl.lire.LireDomain
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireLocalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireIndexFunction
import edu.uwm.cs.pir.domain.SimpleComposition

import net.semanticmetadata.lire.imageanalysis.LireFeature

// Example MIR image query with global features

object TestImageQueryWithComposition extends ExecutionConfig1 with Parallel with Example {
  def main(args: Array[String]): Unit = {
    imageQuery(ParallelVisitor)
  }
}

case class ExecutionConfig1 () {
  var scaleWidth = SCALE_WIDTH
  var scaleHeight = SCALE_HEIGHT
  var indexLocation = INDEX_IMAGE_FEATURE_ROOT
}

trait Example extends Pipeline with ImageQueryFunction[LireFeature] with LireGlobalFeatures with LireLocalFeatures with LireDomain 
      with LireIndexFunction[LireFeature] with SimpleComposition with StringPath {
  
  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)
  
  def imageQuery(v: PipelineVisitor) {
    val img = load (f_image) (FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training"))
    val qImg = load (f_image) (List(SAMPLE_IMAGES_ROOT + "test/1000.jpg"))
    val idx = (img connect f_cedd).join(img connect f_fcth)(f_compose) index f_index

    val x = (qImg connect f_cedd).join(qImg connect f_fcth)(f_compose) query (f_query, idx)

    x.accept(v)
  }

}

