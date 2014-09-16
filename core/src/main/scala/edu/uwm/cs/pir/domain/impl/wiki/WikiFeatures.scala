package edu.uwm.cs.pir.domain.impl.wiki

import edu.uwm.cs.pir.domain.Domain
import edu.uwm.cs.pir.domain.impl.lire.LireDomain
import edu.uwm.cs.pir.domain.WikiFeatures
import scala.collection.JavaConverters._
import edu.uwm.cs.mir.prototypes.proj.wikipedia.utils.XmlParserUtils

trait InternalWikiFeatures extends WikiFeatures with LireDomain {
  type WikiFeatureExtractor = edu.uwm.cs.mir.prototypes.feature.wikipedia.WikiPediaTextAdaptor
  def f_wikiFeatureExtractor: PrjOp[Text, WikiFeatureExtractor] = {
    (text: edu.uwm.cs.mir.prototypes.feature.Text) =>
      {
        //TODO
        //if (awsS3Config.isIs_s3_storage()) wikiPediaDataSetTextExtract.setAWSS3Config(awsS3Config)

        var textContent = ""
        try {
          // filename = filename.substring(filename.lastIndexOf("/") + 1,
          // filename.lastIndexOf("."));
          textContent = XmlParserUtils.getTextFromXmlFileContent(text.getFeature, "text")
          //FileUtils.writeStringToFile(new File(extractedTextFilename), text);
        } catch {
          case e: Exception => println("WARN: File " + text.getId() + " processing problem")
        }
        new edu.uwm.cs.mir.prototypes.feature.wikipedia.WikiPediaTextAdaptor(text.getId, textContent)
      }
  }
}
