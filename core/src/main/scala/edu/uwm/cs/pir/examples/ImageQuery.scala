package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.Constants.SAMPLE_IMAGES_ROOT
import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.SimpleComposition
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.domain.impl.lire.LireDomain
//import edu.uwm.cs.pir.domain.impl.lire.LireLuceneGlobal
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireIndexFunction
import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.pipeline.Sequential
import edu.uwm.cs.pir.utils.FileUtils

import net.semanticmetadata.lire.imageanalysis.LireFeature

object TestSequential extends ExecutionConfig with Sequential with LireImageQuery {
  def main(args: Array[String]): Unit = {
    imageQuery(SequentialVisitor)
  }
}

case class ExecutionConfig () {
  var scaleWidth = 100
  var scaleHeight = 100
  var indexLocation = ""
}

trait LireImageQuery extends Pipeline with SimpleComposition with Loading /*with LireLuceneGlobal*/ with LireGlobalFeatures with LireDomain with LireIndexFunction[LireFeature] with ImageQueryFunction[LireFeature] with StringPath {
  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def imageQuery(v: PipelineVisitor) {
    val img = load (f_image) (FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training"))
    val qImg = load (f_image) (List(SAMPLE_IMAGES_ROOT + "test/1000.jpg"))

    //val cedd = img connect f_cedd connect f_luceneDocTransformer
    val f = (x: Image) => f_cedd(x)

    val idx = (img connect f) index f_index

    val x = (qImg connect f) query (f_query, idx)

    x.accept(v)
    println(x.cache.get)
  }
}
