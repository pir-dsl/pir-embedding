package edu.uwm.cs.pir.domain.features.concrete

/**
 * The example will consume a list of images and desired feature names as input to allow PIR to perform indexing
 * based on provided API name
 *
 */

object DeclarativeTransformation extends App {

  trait SpecEngine {
    def parse(in: String): Spec
  }

  class Key(val name: String, val rawType: String, val featureName: String, val api: String)

  @SerialVersionUID(1L)
  class PIRSpec(val map: Map[String, Any], val innerPIRSpec: PIRSpec) extends Serializable {}

  @SerialVersionUID(1L)
  class Spec(val name: String, val rawType: String, val featureName: String, val api: String, val value: String) {

  }

  trait Transformer {
    def from(spec: Spec): PIRSpec
    def to(pirSpec: PIRSpec): Spec
  }

  case class SimpleTransformer() extends Transformer {
    override def from(spec: Spec): PIRSpec = {
      null
    }
    override def to(pirSpec: PIRSpec): Spec = {
      null
    }
  }

  import com.codahale.jerkson.Json._
  
  // extract 'net.semanticmetadata.lire.imageanalysis.CEDD' feature from '/' with type Image
  // where id = '1' and type = 'jpg' and  api = 'LIRE'
//  val jsonString1 = parse[Map[String, Any]](""" {
//		  "spec" : {
//		  				"extract": {
//		  							"from": {"path" : "/", "type" : "Image"},
//		  							"to": {"type" : "net.semanticmetadata.lire.imageanalysis.CEDD"},
//           							"where" : {"id" : "1", "type" : "jpg", "api" : "LIRE"}
//		  							}
//		  			}
//      }""")
		  			
  // transform CEDD feature to org.apache.lucene.document.Document
  // where id = 'cedd1' and type = 'jpg' and  api = 'LIRE'
//  val jsonString2 = parse[Map[String, Any]](""" {
//		  "spec" : {
//		  				"transform": {
//		  							"from": {"fqn" : "net.semanticmetadata.lire.imageanalysis", "type" : "CEDD"},
//		  							"to": {"type" : "org.apache.lucene.document.Document"},
//           							"where" : {"id" : "cedd1", "api" : "LIRE"}
//		  							}
//		  			}
//      }""")
//		  			
//  println(jsonString1)
//  println(jsonString2)
//  //Testing code
//  val transformer = new SimpleTransformer
//
//  println(transformer.from(new Spec("", "", "", "", "")))

}