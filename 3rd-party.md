Added support for 3rd party libraries, here we use OpenIMAJ as example to integrate in the Video processing support:

1. Copy the below video related jars (Search it in Maven repo and download it) into pir-embedding/core/custom_lib
	 
	 core-1.3.1.jar (this is the base jar that's needed for the rest of the OpenIMAJ projects)
	 core-image-1.3.1.jar
	 core-video-1.3.1.jar
	 core-math-1.3.1.jar
	 core-video-capture-1.3.1.jar
	 gstreamer-video-1.3.1.jar
	 video-processing-1.3.1.jar
	 xuggle-video-1.3.1.jar
	 sanselan-0.97-incubator.jar
	 jai_core-1.1.3.jar
	 sun-jai_codec.jar
	 jama-1.0.2.jar
	 BezierUtils-1.0.0.jar
	
2. Add these jars into your classpath if you want to compile in IDE like Eclipse (this is not needed if you execute things in command line);	 

3. Create a new package in edu.uwm.cs.pir.domain.impl called openimaj and create an empty Scala file there as OpenIMAJFeatures.scala;

4. Then you will create a OpenIMAJDomain trait

trait OpenIMAJDomain extends Domain {
    type Image = org.openimaj.image.MBFImage
    //Don't process Text, so just simply copy the one from LireFeatures
    type Text = edu.uwm.cs.mir.prototypes.feature.Text
    type Video = org.openimaj.video.Video[Image]
}

5. Now it's your freedom to add whatever functionalities as you like, in this example, we will first create a function that can:
   a. Draw some shapes on a given image;
   b. Display this image.
   This is to reflect the OpenIMAJ image tutorial at http://www.openimaj.org/tutorial/processing-your-first-image.html.
   Let's first import all these dependencies
   import org.openimaj.math.geometry.shape.Ellipse
   import org.openimaj.image.colour.RGBColour
   import org.openimaj.image.typography.hershey.HersheyFont
   
   then we see image.drawShapeFilled method takes a shape as parameter, hence we define that as well;
   also, we also see image.drawText method takes 5 parameters, we define that in the methods accordingly
   
   We achieve the below definition at the end
  
trait ImageDisplay extends Domain with Association {
  def f_display: PrjOp[Image, JFrame] // Image => OpenIMAJImageDisplayResult
  def f_drawShapeFilled(shape : Shape): PrjOp[Image, Image]
  def f_drawText(txt: String, x: Int, y: Int, font: HersheyFont, size: Int, color: Array[java.lang.Float]): PrjOp[Image, Image]

  //Just for compilation purpose, no need for the below function in ImageDisplay
  def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID = ???
}
   
   then we can implement the above easily as below:
   
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

With the above code (see edu.uwm.cs.pir.domain.impl.openimaj.OpenIMAJFeatures.scala for details), we are ready to write some test code to see 
what we've got. Please refer edu.uwm.cs.pir.examples.OpenIMAJImageDrawAndDisplay.scala to see the code details. The testing code is very straightforward
and it basically replicates the functionality that the openimaj image tutorial page demos.

P.S. Depending on what functions you plan to integrate into PIR for openIMAJ, you can choose some of the dependencies here:
http://www.openimaj.org/openimaj-core-libs/core-video/dependencies.html
