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

object TestTransmedia extends ExecutionConfigTransmedia with Sequential with Example4 {
  def main(args: Array[String]): Unit = {
    query(SequentialVisitor)
  }
}

trait Example4 extends Pipeline with FeatureLoadFunction with LireFeatures with InternalWikiFeatures with MalletTraining with StringPath {

  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def query(v: PipelineVisitor) {
    val img = load(f_image)(FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training", IMAGE))
    val txt = load(f_text)(FileUtils.pathToFileList(SAMPLE_TEXT_ROOT + "training", TEXT))
    val qImg = load(f_image)(List(SAMPLE_IMAGES_ROOT + "test/query.jpg"))

    val siftImg = img.connect(f_sift)
    val xmlText = txt.connect(f_wikiFeatureExtractor)
    val lda = xmlText.train(f_lda_train)
    val cluster = siftImg.train(f_cluster_train)

    val topic = xmlText.connect(lda)(f_lda_proj)
    val histogram = siftImg.connect(cluster)(f_cluster_proj)

    val composed = topic.join(histogram) // ((x,y)=>(x,y))
    val cca = composed.train(f_cca_train)

    val q = qImg.connect(f_sift).connect(cluster)(f_cluster_proj)

    val x = q.connect(cca)(f_cca_proj2)

    x.accept(v)
    println(x.cache.get)
  }
}