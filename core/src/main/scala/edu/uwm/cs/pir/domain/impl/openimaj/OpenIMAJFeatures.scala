package edu.uwm.cs.pir.domain.impl.openimaj

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.impl.Association
import org.openimaj.math.geometry.shape.Shape
import org.openimaj.image.colour.RGBColour
import org.openimaj.image.typography.hershey.HersheyFont
import javax.swing.JFrame

trait ImageDisplay extends Domain with Association {
  var shape : Shape = null
  var txt: String = null; var x: Int = -1; var y: Int = -1; var font: HersheyFont = null; var size: Int = -1; var color: Array[java.lang.Float] = null

  def f_display: PrjOp[Image, JFrame] // Image => OpenIMAJImageDisplayResult
  def f_drawShapeFilled: PrjOp[Image, Image]
  def f_drawText: PrjOp[Image, Image]
  
  //Just for compilation purpose, no need for the below function in ImageQuery
  def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID = ???
}

trait OpenIMAJDomain extends Domain {
  type Image = org.openimaj.image.MBFImage
  type Text = edu.uwm.cs.mir.prototypes.feature.Text //Don't process Text, so just simply copy the one from LireFeatures
  type Video = org.openimaj.video.Video[Image]
}

trait OpenIMAJFeatures extends ImageDisplay with OpenIMAJDomain {
  def f_display: PrjOp[Image, JFrame] = {
    (image: Image) => {
      org.openimaj.image.DisplayUtilities.display(image)
    }
  }
  def f_drawShapeFilled: PrjOp[Image, Image] = {
    (image: Image) => { 
      image.drawShapeFilled(shape, RGBColour.WHITE) 
      image 
      }
  }

  def f_drawText: PrjOp[Image, Image] = {
    (image: Image) => { 
      image.drawText(txt, x, y, font, size, color) 
      image 
      }
  }
}

trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
  }
}

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}
object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

trait Test {
def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
  val m = implicitly[Monoid[A]]
  val fl = implicitly[FoldLeft[M]]
  fl.foldLeft(xs, m.mzero, m.mappend)
}

def tabulate[T](len: Int, f: Int => T)(implicit m: ClassManifest[T]) = {
    val xs = new Array[T](len)
    for (i <- 0 until len) xs(i) = f(i)
    xs
}

trait MonoidOp[A] {
val F: Monoid[A]
val value: A
def |+|(a2: A) = F.mappend(value, a2)
}

implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
val F = implicitly[Monoid[A]]
val value = a
}
}

