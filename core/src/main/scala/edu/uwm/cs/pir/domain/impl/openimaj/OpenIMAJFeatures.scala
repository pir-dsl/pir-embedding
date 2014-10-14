package edu.uwm.cs.pir.domain.impl.openimaj

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.impl.Association
import org.openimaj.math.geometry.shape.Shape
import org.openimaj.image.colour.RGBColour
import org.openimaj.image.typography.hershey.HersheyFont
import javax.swing.JFrame

trait ImageDisplay extends Domain with Association {
  def f_display: PrjOp[Image, JFrame] // Image => OpenIMAJImageDisplayResult
  def f_drawShapeFilled(shape : Shape): PrjOp[Image, Image]
  def f_drawText(txt: String, x: Int, y: Int, font: HersheyFont, size: Int, color: Array[java.lang.Float]): PrjOp[Image, Image]

  //Just for compilation purpose, no need for the below function in ImageQuery
  def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID = ???
}

trait OpenIMAJDomain extends Domain {
  type Image = org.openimaj.image.MBFImage
  type Text = edu.uwm.cs.mir.prototypes.feature.Text //Don't need to process Text, so just simply copy the one from LireFeatures
  type Video = org.openimaj.video.Video[Image]
}

trait OpenIMAJFeatures extends ImageDisplay with OpenIMAJDomain {

  def f_display: PrjOp[Image, JFrame] = {
    (image: Image) =>
      {
        org.openimaj.image.DisplayUtilities.display(image)
      }
  }

  def f_drawShapeFilled(shape : Shape): PrjOp[Image, Image] = {
    (image: Image) =>
      {
        image.drawShapeFilled(shape, RGBColour.WHITE)
        image
      }
  }

  def f_drawText(txt: String, x: Int, y: Int, font: HersheyFont, size: Int, color: Array[java.lang.Float]): PrjOp[Image, Image] = {
    (image: Image) =>
      {
        image.drawText(txt, x, y, font, size, color)
        image
      }
  }
}

