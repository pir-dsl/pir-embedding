package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.SimpleComposition
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.domain.impl.lire.LireDomain
import edu.uwm.cs.pir.domain.impl.lire.LireFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireIndexFunction

import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.pipeline.Parallel
import edu.uwm.cs.pir.pipeline.Print

import edu.uwm.cs.pir.utils.ExecutionConfig
import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.utils.Constants._

import net.semanticmetadata.lire.imageanalysis.LireFeature

object TestImageQuery extends ExecutionConfig with Parallel with LireImageQuery {
  def main(args: Array[String]): Unit = {
    imageQuery(ParallelVisitor)
  }
}

trait LireImageQuery extends Pipeline with SimpleComposition 
  with LireFeatures with LireDomain with LireIndexFunction[LireFeature] with ImageQueryFunction[LireFeature] with StringPath {
  
  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def imageQuery(v: PipelineVisitor) {
    val img = load (f_image) (FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training", IMAGE))
    val qImg = load (f_image) (List(SAMPLE_IMAGES_ROOT + "test/query.jpg"))

    val f = (x: Image) => f_cedd(x)

    val idx = (img connect f) index f_index

    val x = (qImg connect f) query (f_query, idx)

    x.accept(v)
    //x.cache.get
  }
}
