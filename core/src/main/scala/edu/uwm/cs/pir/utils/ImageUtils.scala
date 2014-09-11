package edu.uwm.cs.pir.utils

import java.io._
import java.net._

import org.apache.commons.io.IOUtils
import java.awt.image.BufferedImage

import javax.imageio._
import javax.imageio.stream._

import scala.util.control.Breaks._

import edu.uwm.cs.pir.utils.GeneralUtils._

object ImageUtils {

  class Dimension(val width: Int, val height: Int) {}

  private def getImageDimension(filePathAndName: String): Dimension = {
    var stream: FileImageInputStream = null
    try {
      stream = new FileImageInputStream(new File(filePathAndName))
    } catch {
      case ex: IOException => log(ex.getMessage())("Warning")
    }
    getImageDimension(stream)
  }

  private def getImageDimension(stream: ImageInputStream): Dimension = {
    var result: Dimension = null
    val suffix = "jpg"
    val iter = ImageIO.getImageReadersBySuffix(suffix)
    if (iter.hasNext()) {
      val reader = iter.next()
      try {
        reader.setInput(stream)
        val width = reader.getWidth(reader.getMinIndex())
        val height = reader.getHeight(reader.getMinIndex())
        result = new Dimension(width, height)
      } catch {
        case ex: IOException => log(ex.getMessage())("Warning")
      } finally {
        reader.dispose
      }
    } else {
      log("No reader found for given format: " + ".jpg")("Warning")
    }
    result
  }

  @throws(classOf[IOException])
  def getBufferedImage(url: URL) = {
    var image: BufferedImage = null
    try {
      val imageInputStream = ImageIO.createImageInputStream(url.openStream())
      val dimension = getImageDimension(imageInputStream)
      if ((dimension != null) && (dimension.width < 5000 && dimension.height < 5000)) {
        image = ImageIO.read(url)
      }
    } catch {
      case ex: Exception => {
        log("Error reading url " + url + " due to " + ex.getMessage())("Warning")
        image = handleGrayScaleImages(image, ImageIO.createImageInputStream(url))
      }
    }
    image
  }

  @throws(classOf[IOException])
  def getBufferedImage(is: InputStream) = {
    var image: BufferedImage = null
    val bytes = IOUtils.toByteArray(is)
    val in = new ByteArrayInputStream(bytes)
    try {
      if (in.markSupported()) {
        in.mark(0)
        val imageInputStream = ImageIO.createImageInputStream(in)
        val dimension = getImageDimension(imageInputStream)
        if ((dimension != null) && (dimension.width < 5000 && dimension.height < 5000)) {
          in.reset()
          image = ImageIO.read(imageInputStream)
        }
      } else {
        image = ImageIO.read(in)
      }
    } catch {
      case ex: Exception => {
        log("Error reading input stream " + is + " due to " + ex.getMessage())("Warning")
        image = handleGrayScaleImages(image, ImageIO.createImageInputStream(is))
      }
    }
    image
  }

  @throws(classOf[IOException])
  def getBufferedImage(filePathAndName: String) = {
    var image: BufferedImage = null
    try {
      val dimension = getImageDimension(filePathAndName)
      if ((dimension != null) && (dimension.width < 5000 && dimension.height < 5000)) {
        image = ImageIO.read(new File(filePathAndName))
      }
    } catch {
      case ex: Exception => {
        log("Error reading file " + filePathAndName + " due to " + ex.getMessage())("Warning")
        image = handleGrayScaleImages(image, filePathAndName)
      }
    }
    image
  }

  @throws(classOf[IOException])
  def readInputAsImage(url: URL) = {
    getBufferedImage(url)
  }

  @throws(classOf[IOException])
  def readInputAsImage(is: InputStream) = {
    getBufferedImage(is)
  }

  @throws(classOf[IOException])
  def readInputAsImage(path: String) = {
    getBufferedImage(path)
  }

  @throws(classOf[IOException])
  private def handleGrayScaleImages(bufferedImage: BufferedImage, path: String): BufferedImage = {
    val stream = ImageIO.createImageInputStream(new File(path))
    getBufferedImage(bufferedImage, stream)
  }

  @throws(classOf[IOException])
  private def handleGrayScaleImages(bufferedImage: BufferedImage, stream: ImageInputStream): BufferedImage = {
    getBufferedImage(bufferedImage, stream)
  }

  @throws(classOf[IOException])
  private def getBufferedImage(bufferedImage: BufferedImage, stream: ImageInputStream): BufferedImage = {
    val iter = ImageIO.getImageReaders(stream)
    var lastException: IOException = null
    var localBufferedImage: BufferedImage = null
    breakable {
      while (iter.hasNext) {
        var reader: ImageReader = null
        try {
          reader = iter.next
          val param = reader.getDefaultReadParam()
          reader.setInput(stream, true, true)
          val imageTypes = reader.getImageTypes(0)
          breakable {
            while (imageTypes.hasNext()) {
              val imageTypeSpecifier = imageTypes.next()
              val bufferedImageType = imageTypeSpecifier.getBufferedImageType()
              if (bufferedImageType == BufferedImage.TYPE_BYTE_GRAY) {
                param.setDestinationType(imageTypeSpecifier)
                break
              }
            }
          }
          localBufferedImage = reader.read(0, param)
          if (null != bufferedImage) {
            break
          }
        } catch {
          case ex: IOException => lastException = ex
        } finally {
          if (null != reader) {
            reader.dispose()
          }
        }
      }
    }
    // If there is not an image at the end of all readers
    if (null == bufferedImage) {
      if (null != lastException) {
        throw new IOException(lastException)
      }
    }
    localBufferedImage
  }
}