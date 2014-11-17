package edu.uwm.cs.pir.examples

import net.semanticmetadata.lire.imageanalysis.sift.Feature

import java.net.URL
import org.openimaj.image.ImageUtilities
import org.openimaj.image.MBFImage
import org.openimaj.feature.local.matcher.BasicMatcher
import org.openimaj.image.feature.local.keypoints.Keypoint

import edu.uwm.cs.pir.domain.StringPath

import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.pipeline.Sequential
import edu.uwm.cs.pir.pipeline.Print

import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.utils.Constants._

import edu.uwm.cs.pir.domain.impl.openimaj.OpenIMAJFeatureMatchingWithLirSift

object TestFeatureMatching extends Sequential with OpenIMAJFeatureMatching {
  def main(args: Array[String]): Unit = {
    featureMatching(SequentialVisitor)
  }
}

trait OpenIMAJFeatureMatching extends Pipeline with OpenIMAJFeatureMatchingWithLirSift with StringPath {
  //This is a trivial example where p is not even used
  def f_image: LoadOp[Path, Image] = (p: Path) => new edu.uwm.cs.mir.prototypes.feature.Image(p)
  def f_mbfImage(p: Path): MBFImage = ImageUtilities.readMBF(new java.io.File(p))

  //def f_displayMBF(matches: org.openimaj.image.Image[MBFImage, Image[MBFImage, Keypoint]]) = DisplayUtilities.display(matches)

  def featureMatching(v: PipelineVisitor) = {

    val qImg = load(f_image)(List(SAMPLE_IMAGES_ROOT + "openimaj/query.jpg"))
    val tImg = load(f_image)(List(SAMPLE_IMAGES_ROOT + "openimaj/target.jpg"))

    val qMBFImg = f_mbfImage(SAMPLE_IMAGES_ROOT + "openimaj/query.jpg")
    val tMBFImg = f_mbfImage(SAMPLE_IMAGES_ROOT + "openimaj/target.jpg")

    val qSift = qImg connect f_sift
    val tSift = tImg connect f_sift

    qSift.accept(v)
    tSift.accept(v)
    
    var qSiftFeature =  qSift.get.asInstanceOf[List[FeatureDoc[List[Feature]]]].map(pair => pair._2)
    var tSiftFeature =  tSift.get.asInstanceOf[List[FeatureDoc[List[Feature]]]].map(pair => pair._2)
    
    val x = qSiftFeature(0)
    val y = tSiftFeature(0)
    
    val queryKeypoints = f_getKeypointList(x)
    val targetKeypoints = f_getKeypointList(y)

    val matches = f_getMatches(qMBFImg, tMBFImg, queryKeypoints, targetKeypoints)

    f_displayMatchedMBFImage(matches)
  }
}
