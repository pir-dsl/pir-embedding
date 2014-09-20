package edu.uwm.cs.pir.domain.impl

import edu.uwm.cs.pir.domain.Domain

/**
 * This is for the use case when features from two different modalities need to be corrrelated with each other while
 * their IDs are different. In this case, this association trait can be used to tie two IDs together
 * See TransmediaQuery.scala for example
 */
trait Association extends Domain {
  var idMap : Map[ID, ID] = Map()
  def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID
}

trait SimpleAssociation extends Association {
  override def obtainAssociatedID[ID, Y]: (ID, Map[ID, Y], Map[ID, ID]) => ID = {
    (id, col, idMap) =>
      {
        val idString = id.asInstanceOf[String]
        var ext = col.head._1.asInstanceOf[String]
        ext = ext.substring(ext.lastIndexOf("."), ext.length)
        val otherID = idMap.get(idString.substring(0, idString.lastIndexOf(".")).asInstanceOf[ID]).get
        if (col.get(otherID) != null) {
          //otherID
          (otherID .asInstanceOf[String] + ext).asInstanceOf[ID]
        } else {
          //If there is no corresponding id in another modality, use the original id
          id
        }
      }
  
  }
}