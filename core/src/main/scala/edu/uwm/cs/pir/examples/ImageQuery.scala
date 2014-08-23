package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.pipeline._
import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.SimpleComposition
import edu.uwm.cs.pir.domain.Composition
import edu.uwm.cs.pir.domain.Loading
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.domain.impl.lire.LireGlobalFeatures
import edu.uwm.cs.pir.domain.impl.lire.LireIndexFunction
import edu.uwm.cs.pir.domain.impl.lire.LireDomain

object TestSequential extends Sequential with LireImageQuery {
  def main(args: Array[String]): Unit = {
    imageQuery(SequentialVisitor)
  }
}

trait LireImageQuery extends Pipeline with SimpleComposition with Loading with LireGlobalFeatures with LireDomain with LireIndexFunction with ImageQueryFunction with StringPath {
  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_text: LoadOp[Path, Text] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Text(p)

  def imageQuery(v: PipelineVisitor) {
    val img = load (f_image) (List("image_3", "image_2", "image_3", "image_3"))
    val qImg = load (f_image) (List("image_3", "image_2"))

    val f = (x: Image) => f_compose(f_cedd(x), f_fcth(x))

    val idx = (img connect f) index f_index

    val x = (qImg connect f) query (f_query, idx)

    x.accept(v)
    println(x.cache.get)
  }
}
