package edu.uwm.cs.pir.examples

import java.net.URL
import org.openimaj.image.ImageUtilities
import org.openimaj.math.geometry.shape.Ellipse
import org.openimaj.image.colour.RGBColour
import org.openimaj.image.typography.hershey.HersheyFont
import org.openimaj.math.geometry.shape.Shape

import edu.uwm.cs.pir.domain.StringPath
import edu.uwm.cs.pir.domain.impl.openimaj.OpenIMAJFeatures

import edu.uwm.cs.pir.pipeline.Pipeline
import edu.uwm.cs.pir.pipeline.Sequential
import edu.uwm.cs.pir.pipeline.Print

import edu.uwm.cs.pir.utils.FileUtils
import edu.uwm.cs.pir.utils.Constants._

import net.semanticmetadata.lire.imageanalysis.LireFeature

object TestImageDrawAndDisplay extends Sequential with OpenIMAJImageDrawAndDisplay {
  def main(args: Array[String]): Unit = {
    imageDrawAndDisplay(SequentialVisitor)
  }
}

trait OpenIMAJImageDrawAndDisplay extends Pipeline with OpenIMAJFeatures with StringPath {
  //This is a trivial example where p is not even used
  def f_image: LoadOp[Path, Image] = (p: Path) => ImageUtilities.readMBF(new java.io.File(p))
  def imageDrawAndDisplay(v: PipelineVisitor) {
    val shapes = List(new Ellipse(700f, 450f, 20f, 10f, 0f), new Ellipse(650f, 425f, 25f, 12f, 0f), 
    new Ellipse(600f, 380f, 30f, 15f, 0f), new Ellipse(500f, 300f, 100f, 70f, 0f))
    val texts = List(("OpenIMAJ is", 425, 300, HersheyFont.ASTROLOGY, 20, RGBColour.BLACK), ("Awesome", 425, 330, HersheyFont.ASTROLOGY, 20, RGBColour.BLACK))    
    var img = load(f_image)(List(SAMPLE_IMAGES_ROOT + "openimaj/sinaface.jpg"))
    shapes.map(shape => img = img connect f_drawShapeFilled(shape))
    texts.map(text => img = img connect f_drawText(text._1, text._2, text._3, text._4, text._5, text._6))
    val display = img connect f_display
    display.accept(v)
  }
}
