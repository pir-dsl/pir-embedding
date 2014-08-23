package edu.uwm.cs.pir.domain.impl.lire

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.GlobalFeatures
import edu.uwm.cs.pir.domain.LocalFeatures
import edu.uwm.cs.pir.domain.Indexing
import edu.uwm.cs.pir.domain.Training

trait LireDomain extends Domain {
  type Image = edu.uwm.cs.mir.prototypes.feature.Image
  type Text = edu.uwm.cs.mir.prototypes.feature.Text
}

trait LireIndexFunction extends Indexing {
  type Index[X] = String;		

  def f_index[X] = (s: List[(ID, X)]) => "Index"
  def f_query[X] = (k: X, i: Index[X]) => List()
}

trait LireTraining extends Training

trait LireGlobalFeatures extends GlobalFeatures {
  
  type CEDD = net.semanticmetadata.lire.imageanalysis.CEDD
  type FCTH = net.semanticmetadata.lire.imageanalysis.FCTH
  def f_cedd: PrjOp[Image, CEDD] = {
    (image: Image) => new net.semanticmetadata.lire.imageanalysis.CEDD()
  }  
  def f_fcth: PrjOp[Image, FCTH] = {
    (image: Image) => new net.semanticmetadata.lire.imageanalysis.FCTH()
  }
}

trait LireLocalFeatures extends LocalFeatures