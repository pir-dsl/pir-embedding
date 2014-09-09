package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.Constants._

import edu.uwm.cs.pir.domain.impl.lire.LireDomain
import edu.uwm.cs.pir.domain.FeatureLoadFunction
import edu.uwm.cs.pir.domain.SFAFunction
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireLocalFeatures
import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.domain.impl.lire.LireDomain
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalSimilarity
import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.pipeline.Parallel
import net.semanticmetadata.lire.imageanalysis.LireFeature
import edu.uwm.cs.pir.utils.FileUtils

object TestSFA extends ExecutionConfig2 with Parallel with Example3 {
  def main(args: Array[String]): Unit = {
    query(ParallelVisitor)
  }
}

case class ExecutionConfig2() {
  var scaleWidth = SCALE_WIDTH
  var scaleHeight = SCALE_HEIGHT
}

trait Example3 extends Pipeline with FeatureLoadFunction with SFAFunction with LireGlobalFeatures
  with LireLocalFeatures with LireGlobalSimilarity with LireDomain with StringPath {

  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def query(v: PipelineVisitor) {
	  
    val qImg = f_image(SAMPLE_IMAGES_ROOT + "test/1000.jpg")

    val img = load (f_image) (FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training"))
    val x = img connect f_colorLayout connect f_distance(f_colorLayout(qImg)) sort f_order top 8

    def f_filter(l: List[(ID, _)]) = { val idl = l.map(e => e._1); (x: (ID, _)) => idl.contains(x._1) }

    val y = img.filter(f_filter, x) connect f_cedd connect f_distance(f_cedd(qImg)) sort f_order top 5

    val z = img.filter(f_filter, y) connect f_gabor connect f_distance(f_gabor(qImg)) sort f_order top 2

    val r = img.filter(f_filter, z)

    r.accept(v)
    println(x.cache.get)
    println(y.cache.get)
    println(z.cache.get)
    println(r.cache.get)
  }
}