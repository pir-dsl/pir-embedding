package edu.uwm.cs.pir.utils

import java.io._

import edu.uwm.cs.mir.prototypes.feature._
import edu.uwm.cs.mir.prototypes.aws.AWSS3Config
import edu.uwm.cs.mir.prototypes.utils.Utils._

object GeneralUtils {

  val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

  def time[A](block: => A)(algorithm: String, storeResult: Boolean = false): A = {
    val now = System.currentTimeMillis
    val result = block
    if (storeResult) {
      val micros = (System.currentTimeMillis - now)
      val filenameHeader = if (!algorithm.contains("(")) algorithm
      else algorithm.substring(0, algorithm.indexOf("("))
      val writer = new PrintWriter(
        new File(
          "output/" + filenameHeader + "-" +
            formatter.format(new java.util.Date(System.currentTimeMillis)) + ".txt"))
      writer.write("Execution Time: %d microseconds\n".format(micros))
      writer.close()
    }
    result
  }

  def chunkList[In <: IFeature](list: List[In], chunkSize: Int): List[List[In]] = {
    if (list.size <= chunkSize) List(list)
    else {
      val (head, tail) = list.splitAt(chunkSize)
      head :: chunkList(tail, chunkSize)
    }
  }

  implicit def isDebug = true

  def log(msg: String)(implicit level: String = "DEBUG", isDebug: Boolean = false): Unit = {
    if (("INFO" == level) || (isDebug)) {
      println(level + ": " + msg);
    }
  }

  @throws(classOf[IOException])
  class ClassLoaderObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
    @throws(classOf[IOException])
    @throws(classOf[ClassNotFoundException])
    override def resolveClass(desc: ObjectStreamClass): Class[_] =
      {
        try {
          super.resolveClass(desc)
        } catch {
          case e: ClassNotFoundException => {
            val classpath = System.getProperty("java.class.path") 
            log("classpath = " + classpath)("INFO")
            log("current location = " + this.getClass.getProtectionDomain.getCodeSource.getLocation)
            addJarToClasspath(this.getClass.getProtectionDomain.getCodeSource.getLocation)
            log("desc.getName = " + desc.getName)("INFO")
            super.resolveClass(desc)
            /*val clazz = ClassLoader.getSystemClassLoader.loadClass(desc.getName);
            log("class = " + clazz)("INFO")
            clazz*/
          }
        }
      }
  }
  
}
