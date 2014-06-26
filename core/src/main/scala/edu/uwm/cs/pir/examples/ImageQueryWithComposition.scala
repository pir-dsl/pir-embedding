package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.domain._
import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.pipeline.Pipeline

// Example MIR image query with global features

trait Example extends Pipeline with ImageQueryFunction with Composition with StringPath {
  
  def imageQuery(v: PipelineVisitor) {
    val img = f_image(List("image_3", "image_2", "image_3", "image_3"))
    val qImg = f_image(List("image_3", "image_2"))
    val idx = (img connect f_cedd).join(img connect f_fcth)(f_compose) index f_index

    val x = (qImg connect f_cedd).join(qImg connect f_fcth)(f_compose) query (f_query, idx)

    x.accept(v)
    println(x.cache.get)
  }

}

