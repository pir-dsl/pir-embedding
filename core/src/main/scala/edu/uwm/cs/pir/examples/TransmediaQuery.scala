package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.pipeline._
import edu.uwm.cs.pir.domain._

object TestTransmedia extends Sequential with NumberFunction with Example4 {
   def main(args: Array[String]): Unit = {
    query(SequentialVisitor)
  }
}

trait Example4 extends Pipeline with Loading with GlobalFeature with LocalFeature with Training with StringPath {
   def query(v: PipelineVisitor) {
     val img = f_image(List("image_4", "image_2", "image_5", "image_3", "image_6"))
     val txt = f_text(List("text_4", "text_2", "text_5", "text_3", "text_6"))
     val qImg = f_image(List("image_3"))
     
     val siftImg = img.connect(f_sift)
     val lda = txt.train(f_lda_train) 
     val cluster = siftImg.train(f_cluster_train)
     
     val topic = txt.connect(lda)(f_lda_proj)
     val histogram = siftImg.connect(cluster)(f_cluster_proj)
     
     val composed = topic.join(histogram) // ((x,y)=>(x,y))
     val cca = composed.train(f_cca_train)
     
     val q =  qImg.connect(f_sift).connect(cluster)(f_cluster_proj)
 
     val x = q.connect(cca)(f_cca_proj2)
   
     x.accept(v)
     println(x.cache.get)
   }
}