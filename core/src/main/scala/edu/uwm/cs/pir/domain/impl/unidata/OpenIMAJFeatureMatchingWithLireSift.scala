package edu.uwm.cs.pir.domain.impl.unidata

import scala.collection.JavaConversions._

import edu.uwm.cs.pir.DataType
import edu.uwm.cs.pir.LireToMapTranformer
import edu.uwm.cs.pir.MapToOpenIMAJTranformer
import edu.uwm.cs.pir.domain.impl.lire.LireLocalFeatures

import edu.uwm.cs.pir.utils.Constants._

import net.semanticmetadata.lire.imageanalysis.sift.Feature

import org.openimaj.feature.local.list.LocalFeatureList
import org.openimaj.image.feature.local.keypoints.Keypoint
import org.openimaj.feature.local.list.MemoryLocalFeatureList
import org.openimaj.feature.local.matcher.MatchingUtilities
import org.openimaj.math.geometry.transforms.estimation.RobustAffineTransformEstimator
import org.openimaj.math.model.fit.RANSAC
import org.openimaj.feature.local.matcher.consistent.ConsistentLocalFeatureMatcher2d
import org.openimaj.feature.local.matcher.FastBasicKeypointMatcher
import org.openimaj.image.MBFImage
import org.openimaj.image.colour.RGBColour
import org.openimaj.image.DisplayUtilities

trait OpenIMAJFeatureMatchingWithLireSift extends LireLocalFeatures {
  //Just for compilation purpose, no need for the below function in ImageDisplay
  def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID = ???
  
  def f_getKeypointList (in : List[Feature]) : LocalFeatureList[Keypoint] = {
    val dataTypeRep = in.map(feature => LireToMapTranformer.transformFromLireSiftFeature(feature))
    val lst = dataTypeRep.map(elem => MapToOpenIMAJTranformer.transformToOpenIMAJKeyPoint(elem))
    new MemoryLocalFeatureList(lst.map(elem => elem.getA))
  }
  
  def f_displayMatchedMBFImage(image : MBFImage) = {
    DisplayUtilities.display(image)
  } 
  
  def f_getMatches (qMBFImg : MBFImage, tMBFImg : MBFImage, queryKeypoints : LocalFeatureList[Keypoint], targetKeypoints : LocalFeatureList[Keypoint] ) = {
        /*Below is matcher version 1 from http://www.openimaj.org/tutorial/sift-and-feature-matching.html */
    //    val matcher = new BasicMatcher[Keypoint](80)
    //    matcher.setModelFeatures(queryKeypoints)
    //    matcher.findMatches(targetKeypoints)

    /*Below is matcher version 2 from http://www.openimaj.org/tutorial/sift-and-feature-matching.html */
    val modelFitter = new RobustAffineTransformEstimator(5.0, 1500, new RANSAC.PercentageInliersStoppingCondition(0.5))
    val matcher = new ConsistentLocalFeatureMatcher2d[Keypoint](new FastBasicKeypointMatcher[Keypoint](8), modelFitter)

    matcher.setModelFeatures(queryKeypoints)
    matcher.findMatches(targetKeypoints)
  
    /*Below is basic matcher version from http://www.openimaj.org/tutorial/sift-and-feature-matching.html */
    //val matches = MatchingUtilities.drawMatches(qMBFImg, tMBFImg, matcher.getMatches(), RGBColour.RED)

    /*Below is consistent matcher version from http://www.openimaj.org/tutorial/sift-and-feature-matching.html */
    val matches = MatchingUtilities.drawMatches(qMBFImg, tMBFImg, matcher.getMatches, RGBColour.RED)
    matches
  }
}


