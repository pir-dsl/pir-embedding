package edu.uwm.cs.pir

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import edu.uwm.cs.mir.prototypes.index._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PIRGenericSuite extends FunSuite {
    
//  test ("InvertedIndexSearchResultSorting") {
//    val array = List(InvertedIndexSearchResult(1, "docId1", "result1", 8.1), 
//        InvertedIndexSearchResult(2, "docId2", "result2", 3.5),
//    	InvertedIndexSearchResult(3, "docId3", "result3", 4.6),
//    	InvertedIndexSearchResult(4, "docId4", "result4", 7.8), 
//    	InvertedIndexSearchResult(5, "docId5", "result5", Double.NaN)).sortWith(_>_)
//    array.map(elem => println(elem))
//  }
//
//  test("InvertedIndex Testing") {
//    val index = new InvertedIndex(new Tokenizer)
//    index.index("id1", "content1")
//    index.index("id2", "content2")
//    index.index("id3", "content3")
//    
//    serializeObject(index, awsS3Config, "testId", true)
//    
//    val result = deSerializeObject("testId", awsS3Config, true).asInstanceOf[IIndex]
//    
//    println
//  }
}
