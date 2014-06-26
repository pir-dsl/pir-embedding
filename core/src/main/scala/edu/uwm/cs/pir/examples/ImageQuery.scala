package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.pipeline._
import edu.uwm.cs.pir.domain._

object TestSequential extends Sequential with NumberFunction with ComparableComposition with Example2 {
  def main(args: Array[String]): Unit = {
    imageQuery(SequentialVisitor)
  }
}

trait Example2 extends Pipeline with ImageQueryFunction with Composition with StringPath {
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
