package edu.uwm.cs.pir.utils

import java.io.IOException
import java.util.Map

import net.semanticmetadata.lire.DocumentBuilder
import net.semanticmetadata.lire.DocumentBuilderFactory
import net.semanticmetadata.lire.imageanalysis.LireFeature
import net.semanticmetadata.lire.utils.LuceneUtils

import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.CorruptIndexException
import org.apache.lucene.index.IndexWriter

object IndexUtils {

  @throws(classOf[CorruptIndexException])
  @throws(classOf[IOException])
  def addOneDocToExistingIndex(indexWriter: IndexWriter, doc: Document) : Unit = {
    indexWriter.addDocument(doc)
  }

  def createIndexWithAllDocs(indexLocation: String, docMap: Map[String, Document]) : Unit = {
    var indexWriter: IndexWriter = null
    try {
      indexWriter = LuceneUtils.createIndexWriter(indexLocation, true)
    } catch {
      case ex: IOException => throw new RuntimeException(ex)
    } finally {
      if (indexWriter != null) {
        try {
          indexWriter.addDocuments(docMap.values())
          indexWriter.close()
        } catch {
          case ex: Exception => throw new RuntimeException(ex)
        }
      }
    }
  }

  def getLuceneDocument(id: String, lireFeature: LireFeature, dataType: String): Document = {
    val doc = new Document
    var fieldName: String = null
    if ("CEDD".equalsIgnoreCase(dataType)) {
      fieldName = DocumentBuilder.FIELD_NAME_CEDD
    } else if ("FCTH".equalsIgnoreCase(dataType)) {
      fieldName = DocumentBuilder.FIELD_NAME_FCTH
    } else {
      fieldName = "UNSUPPORTED_FIELD_NAME"
    }
    //In some rare scenario,  lireFeature is null and hence we just pass in a null object in this case
    if (lireFeature == null) {
      doc.add(new StoredField(fieldName, Array[Byte]()))
    } else {
      doc.add(new StoredField(fieldName, lireFeature.getByteArrayRepresentation()))
    }
    if (id != null) doc.add(new TextField(DocumentBuilder.FIELD_NAME_IDENTIFIER, id, Field.Store.YES))
    return doc
  }

  def getLireDocumentBuilder(docType: String): DocumentBuilder = {
    var builder: DocumentBuilder = null
    if ("CEDD".equalsIgnoreCase(docType)) {
      builder = DocumentBuilderFactory.getCEDDDocumentBuilder()
    } else if ("FCTH".equalsIgnoreCase(docType)) {
      builder = DocumentBuilderFactory.getFCTHDocumentBuilder()
    }
    builder
  }

}