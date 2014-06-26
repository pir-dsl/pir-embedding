package edu.uwm.cs.pir

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import edu.uwm.cs.mir.prototypes.feature._
import edu.uwm.cs.mir.prototypes.utils._
import edu.uwm.cs.mir.prototypes.proj.lucene._
import edu.uwm.cs.mir.prototypes.composer._
import edu.uwm.cs.mir.prototypes.index._
import edu.uwm.cs.mir.prototypes.utils.Utils._
import org.scalatest.junit.JUnitRunner
import edu.uwm.cs.pir.utils.GeneralUtils._


@RunWith(classOf[JUnitRunner])
class ProjectorTestSuite extends FunSuite {

//  val img = new Image(Constants.SAMPLE_IMAGES_ROOT + "test/1000.jpg")
//  val colorLayout = f_colorLayout.apply(img)
//  val edgeHistogram = f_edgeHistogram.apply(img)
//  val gabor = f_gabor.apply(img)

//  test("Connectivity test") {
//    assert(colorLayout != null)
//    assert(edgeHistogram != null)
//    assert(gabor != null)
//  }
  
  /*test ("Sorting") {
    val array = List(3,7,5,2).toArray.sortWith((e1, e2) => e1 < e2)
    array.map(elem => print(elem))
  }*/
  
  test ("CL") {
    var classpath = System.getProperty("java.class.path") 
    log("classpath = " + classpath)("INFO")
    var path = this.getClass.getProtectionDomain.getCodeSource.getLocation
    log("current location = " + path)("INFO")
    addJarToClasspath(this.getClass.getProtectionDomain.getCodeSource.getLocation)
    classpath = System.getProperty("java.class.path") 
    log("classpath = " + classpath)("INFO")
  }
}