package edu.uwm.cs.pir.examples

import java.io.File

import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.impl.lire.LireFeatures
import edu.uwm.cs.pir.domain.impl.mallet.MalletTraining
import edu.uwm.cs.pir.domain.impl.wiki.InternalWikiFeatures
import edu.uwm.cs.pir.domain.Training
import edu.uwm.cs.pir.domain.FeatureLoadFunction
import edu.uwm.cs.pir.domain.StringPath

import edu.uwm.cs.pir.pipeline._

import edu.uwm.cs.pir.utils.ExecutionConfigTransmedia
import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.utils.Constants._

object TestSiftImageQuery extends ExecutionConfigTransmedia with Parallel with Example4 {
  def main(args: Array[String]): Unit = {
    query(ParallelVisitor)
  }
}

trait Example5 extends Pipeline with FeatureLoadFunction with LireFeatures with InternalWikiFeatures with MalletTraining with StringPath {

  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def query(v: PipelineVisitor) {
    val img = load(f_image)(FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training", IMAGE))
    
    val siftImg = img.connect(f_sift)
    val cluster = siftImg.train(f_cluster_train)
    val histogram = siftImg.connect(cluster)(f_cluster_proj)
    histogram.accept(v)
  }
}