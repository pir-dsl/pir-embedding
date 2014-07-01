package edu.uwm.cs.pir.domain

import edu.uwm.cs.pir.spark.SparkObject._
import edu.uwm.cs.pir.utils.GeneralUtils._

import java.awt.image.BufferedImage
import java.io.IOException
import java.io.InputStream

import com.amazonaws.services.s3.AmazonS3

import edu.uwm.cs.mir.prototypes.feature.lire.LireFeatureAdaptor
import edu.uwm.cs.pir.utils.AWSS3API
import edu.uwm.cs.pir.utils.AWSS3API.AWSS3Config
import edu.uwm.cs.pir.utils.ImageUtils

import edu.uwm.cs.pir.domain.impl.lire._
import edu.uwm.cs.pir.domain.features.concrete._

//Test implementation of the MIR domain functions
case class MIRFunction (val scaleWidth : Int, val scaleHeight : Int) extends ImageQueryFunction with ContainerFeature {

  type ID = String

  type Image = LireImage
  type Text = LireText

  type CEDD = CEDDWrapper
  type FCTH = FCTHWrapper
  
  type Index[X] = String;  type Index2[X, Y] = String;		

  type Path = Location
  def f_image = (p: Location) => {
    if (p.url.isEmpty) {
      null
    } else {
      val image = new LireImage(p)
      log("load image : " + p.url)("INFO")
      image
    }
  }

  def f_text = (p: Path) => {
    if (p.url.isEmpty) {
      null
    } else {
      val text = new LireText(p)
      log("load text : " + p)("INFO")
      text
    }
  }

  val cedd = new LireCEDD(scaleWidth, scaleHeight)
  val fcth = LireFCTH(scaleWidth, scaleHeight)

  def f_cedd = (i: Image) => {
    log("Apply CEDD to " + i)("INFO")
    cedd.getFeature(i)

  }
  def f_fcth = (i: Image) => {
    log("Apply FCTH to " + i)("INFO")
    fcth.getFeature(i)
  }
  def f_index[X] = (s: List[(ID, X)]) => "Index"
  def f_query[X] = (k: X, i: Index[X]) => List()
}