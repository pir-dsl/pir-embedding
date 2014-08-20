package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.pipeline._
import edu.uwm.cs.pir.domain._

object TestSFA extends Sequential with NumberFunction with SimpleComposition with Example3 {
  def main(args: Array[String]): Unit = {
    query(SequentialVisitor)
  }
}

trait Example3 extends Pipeline with SFAFunction with StringPath {
  def query(v: PipelineVisitor) {
    val qImg = f_image("image_3") 
//    val gabor = f_gabor(qImg)
//    val color = f_colorlayout(qImg)
//    val cedd = f_cedd(qImg)

    val img = f_image(List("image_4", "image_2", "image_5", "image_3", "image_6"))
    val x = img connect f_colorlayout connect f_distance(f_colorlayout(qImg)) sort f_order top 4

    def f_filter(l: List[(ID, _)]) = { val idl = l.map(e => e._1); (x: (ID, _)) => idl.contains(x._1) }

    val y = img.filter(f_filter, x) connect f_cedd connect f_distance(f_cedd(qImg)) sort f_order top 3

    val z = img.filter(f_filter, y) connect f_gabor connect f_distance(f_gabor(qImg)) sort f_order top 2

    val r = img.filter(f_filter, z)

    r.accept(v)
     println(x.cache.get)
      println(y.cache.get)
       println(z.cache.get)
    println(r.cache.get)
  }
}