package edu.uwm.cs.pir.domain.features.concrete

import java.util.Properties

trait Configuration {
    val values : List[Any]
	val properties : Properties
}

trait MFeature[X] extends Cloneable with Serializable {
  val data : X
  val conf : Configuration
}

//Marker interface
trait Indexable {}

//Marker interface
trait Trainable {}

trait MLIn[X, Y] extends MFeature[X] with MLTransform [X, Y] with Trainable {
   def getMLInData : Y
}

trait IndexIn[X, Y] extends MFeature[X] with IndexTransform [X, Y] with Indexable {
   val getIndexInData : Y
}

trait Transform[In, Out] {
  def transform (in : In) : Out
}

trait MLTransform[X, Y] extends Transform[MFeature[X], MLIn[X, Y]] {
  override def transform (in : MFeature[X]) : MLIn[X, Y]
}

trait IndexTransform[X, Y] extends Transform[MFeature[X], IndexIn[X, Y]] {
  override def transform (in : MFeature[X]) : IndexIn[X, Y]
}

object TestApp extends App {
  class TestMLIn[Int] (dataP : Int, confP : Configuration) extends MLIn[Int, String] {
    override val data : Int = dataP
    override val conf : Configuration = confP
    override def getMLInData = data.toString
    override def transform (in : MFeature[Int]) : MLIn[Int, String] = {printData; printMLData; this}
    def printData = println(data)
    def printMLData = {
      val keys = conf.properties.propertyNames
      while (keys.hasMoreElements) {
        println(conf.properties.getProperty(keys.nextElement.asInstanceOf[String]))
      }
    }
  }
  
  val t = new TestMLIn(3, new Configuration{
    override val values  = List(1,2,3)
	override val properties  = {
      val properties = new Properties
      properties.put("k1", "v1")
      properties.put("k2", "v2")
      properties.put("k3", "v3")
      properties
      }
  })
  
  t.transform(t)
}