package edu.uwm.cs.pir.domain.impl.mallet

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.Training
import scala.collection.JavaConverters._

trait MalletTraining extends Training {

  type Histogram[X]
  type Cluster[X]

  def f_cluster_train[X]: TrnOp[X, Cluster[X]] = ???// List[(ID, X)] => Cluster[X]
  def f_cluster_proj[X]: DPrjOp[X, Histogram[X], Cluster[X]] = ???// (X, Cluster[X]) => Histogram[X]
  
  def f_lda_train[X]: TrnOp[X, Topic[X]] = ???// List[(ID, X)] => Topic[X]
  def f_lda_proj[X]: DPrjOp[X, Distribution[X], Topic[X]] =  ???// (X, Topic[X]) => Distribution[X]  
  
  def f_cca_train[X, Y]: TrnOp[(X, Y), CCA[X, Y]] = ???// (List[(ID, (X, Y))]) => CCA[X, Y]
  def f_cca_proj1[X, Y]: DPrjOp[X, List[ID], CCA[X, Y]] = ???// (X, CCA[X, Y]) => List[ID]
  def f_cca_proj2[X, Y]: DPrjOp[Y, List[ID], CCA[X, Y]] = ???// (Y, CCA[X, Y]) => List[ID]
}
