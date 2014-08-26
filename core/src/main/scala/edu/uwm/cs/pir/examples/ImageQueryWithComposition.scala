package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.domain._
import edu.uwm.cs.pir.domain.ImageQueryFunction
import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.pipeline.Pipeline

import edu.uwm.cs.pir.Constants.SAMPLE_IMAGES_ROOT
import edu.uwm.cs.pir.utils.FileUtils

import net.semanticmetadata.lire.imageanalysis.LireFeature

// Example MIR image query with global features

trait Example extends Pipeline with ImageQueryFunction[LireFeature] with Composition with StringPath {
  
  def imageQuery(v: PipelineVisitor) {
    val img = load (f_image) (FileUtils.pathToFileList(SAMPLE_IMAGES_ROOT + "training"))
    val qImg = load (f_image) (List(SAMPLE_IMAGES_ROOT + "test/1000.jpg"))
    val idx = (img connect f_cedd).join(img connect f_fcth)(f_compose) index f_index

    val x = (qImg connect f_cedd).join(qImg connect f_fcth)(f_compose) query (f_query, idx)

    x.accept(v)
    println(x.cache.get)
  }

}

