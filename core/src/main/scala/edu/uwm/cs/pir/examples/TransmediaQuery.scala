package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.impl.lire.LireFeatures
import edu.uwm.cs.pir.domain.impl.mallet.MalletTraining
import edu.uwm.cs.pir.domain.impl.wiki.InternalWikiFeatures
import edu.uwm.cs.pir.domain.Training
import edu.uwm.cs.pir.domain.FeatureLoadFunction
import edu.uwm.cs.pir.domain.StringPath

import edu.uwm.cs.pir.pipeline._

import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.utils.Constants._

object TestTransmedia extends ExecutionConfig3 with Sequential with Example4 {
  def main(args: Array[String]): Unit = {
    query(SequentialVisitor)
  }
}

case class ExecutionConfig3() {
  //CEDD feature env
  var scaleWidth = SCALE_WIDTH
  var scaleHeight = SCALE_HEIGHT
  //Clustering env
  var numberOfClusters = NUM_OF_CLUSTERS
  var clusterFilename = CLUSTER_FIlE
  //LDA env
  var ldaModelFile = LDA_MODEL_FIlE
  var descFile = EXTRACTED_TEXT_FILE_ROOT + "ALL_DESC.txt"
  var stopwordFile = STOPWORDS_ROOT + "en.txt"
  var numberOfTopics = NUM_OF_TOPICS
  var alphaSum = ALPHA_SUM
  var betaW = BETA_W
  var numberOfSamplers = NUMBER_SAMPLER
  var numberOfIterations = NUMBER_ITERATION
  var encoding = DEFAULT_ENCODING
  //LDA env
  var gibbsSamplingIteration = GIBBS_SAMPLING_ITERATION
  var gibbsSamplingThinning = GIBBS_SAMPLING_THINNING
  var gibbsSamplingBurnin = GIBBS_SAMPLING_BURNIN
  
}

trait Example4 extends Pipeline with FeatureLoadFunction with LireFeatures with InternalWikiFeatures with MalletTraining with StringPath {

  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def query(v: PipelineVisitor) {
    val img = load(f_image)(FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training"))
    val txt = load(f_text)(FileUtils.pathToFileList(SAMPLE_TEXT_ROOT + "training"))
    val qImg = load(f_image)(List(SAMPLE_IMAGES_ROOT + "test/query.jpg"))

    val siftImg = img.connect(f_sift)
    val lda = txt.connect(f_wikiFeatureExtractor).train(f_lda_train)
    val cluster = siftImg.train(f_cluster_train)

    val topic = txt.connect(f_wikiFeatureExtractor).connect(lda)(f_lda_proj)
    val histogram = siftImg.connect(cluster)(f_cluster_proj)

    val composed = topic.join(histogram) // ((x,y)=>(x,y))
    val cca = composed.train(f_cca_train)

    val q = qImg.connect(f_sift).connect(cluster)(f_cluster_proj)

    val x = q.connect(cca)(f_cca_proj2)

    x.accept(v)
    println(x.cache.get)
  }
}