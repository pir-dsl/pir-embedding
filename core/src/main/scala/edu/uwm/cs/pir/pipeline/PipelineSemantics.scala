package edu.uwm.cs.pir.pipeline

import scala.collection.parallel.immutable.ParSeq
import edu.uwm.cs.pir.domain.impl.Association
import edu.uwm.cs.pir.domain.StringID

// Test implementation of pipeline visitor to print out string representation of the pipeline

trait Print extends Stage {
  type Col[X] = String
  type Idx[X] = String
  type Trn[X] = String
  type Prj[X] = String

  object PrintVisitor extends StageVisitor {

    // visit stages
    def visit[P, X](l: Source[X], loader: LoadOp[P, X], path: Dir[P]) = l.testAndSet(() =>
      "Load (" + loader.toString + ") from (" + path + ")")

    def visit[X, Y](p: Proj[X, Y], proj: PrjOp[X, Y]) = p.testAndSet(() =>
      "Proj (" + proj.toString + ")")

    def visit[X, Y, M](p: Proj[X, Y], proj: DPrjOp[X, Y, M], train: Train[M]) = p.testAndSet(() => {
      train.accept(this)
      "Proj (" + proj.toString + ") with model (" + train.get + ")"
    })
    def visit[X, Y, I](p: Proj[X, Y], query: DPrjOp[X, Y, I], index: Index[I]) = p.testAndSet(() => {
      index.accept(this)
      "Proj (" + query.toString + ") with index (" + index.get + ")"
    })
    def visit[X, M](t: Train[M], train: TrnOp[X, M], source: Source[X]) = t.testAndSet(() => {
      source.accept(this)
      "Train(" + train.toString + ") with (" + source.get + ")"
    })
    def visit[X, I](i: Index[I], indexer: IdxOp[X, I], source: Source[X]) = i.testAndSet(() => {
      source.accept(this)
      "Index(" + indexer.toString + ") with (" + source.get + ")"
    })

    // visit pipes   
    def visit[X, Y](s: SourcePipe[X, Y]) = s.testAndSet(() => {
      s.left.accept(this)
      s.right.accept(this)
      s.left.get + " => " + s.right.get
    })
    def visit[X, L](f: FilterPipe[X, L]) = f.testAndSet(() => {
      f.left.accept(this); f.list.accept(this)
      f.left.get + " Filter " + f.filter.toString + " with " + f.list.get
    })
    def visit[X](s: SortPipe[X]) = s.testAndSet(() => {
      s.left.accept(this)
      s.left.get + " Sort " + s.right
    })
    def visit[X](t: TopPipe[X]) = t.testAndSet(() => {
      t.left.accept(this)
      t.left.get + " Top " + t.n
    })
    def visit[X, Y, C](j: JoinPipe[X, Y, C]) = j.testAndSet(() => {
      j.left.accept(this)
      j.right.accept(this)
      j.left.get + " Join " + j.right.get + " with " + j.op
    })
    def visit[X, Y, Z](p: ProjPipe[X, Y, Z]) = p.testAndSet(() => {
      p.left.accept(this)
      p.right.accept(this)
      p.left.get + " o " + p.right.get
    })
  }
}

trait IDAssociationHelper extends Association with StringID {
  def constructListWithAssociatedID[X, Y, C](left: Map[ID, X], right: Map[ID, Y], op: CpsOp[X, Y, C]): List[FeatureDoc[C]] = {
    if (left.size != right.size) throw new RuntimeException("The two inputs for join operation must have the same size!")
    val list = left.map(
      elem => {
        val associatedID = obtainAssociatedID[ID, Y](elem._1, right, idMap)
        (elem._1 + ":" + associatedID, op(elem._2, right.get(associatedID).get))
      }).toList
    list
  }
}

trait Sequential extends Stage with IDAssociationHelper {
  type Col[X] = List[FeatureDoc[X]]
  type Idx[X] = X
  type Trn[X] = X
  type Prj[X] = X

  // Index is generated by the pipeline semantics while it is abstract to domain functions

  object SequentialVisitor extends StageVisitor {

    // visit stages
    def visit[P, X](l: Source[X], loader: LoadOp[P, X], path: Dir[P]) = l.testAndSet(() =>
      path.map(file =>
        {
          val id = file.toString
          (id.substring(id.lastIndexOf('/') + 1, id.length), loader(file))
        }) //.zipWithIndex.map(p=>(p._2, p._1))
        )

    def visit[X, Y](p: Proj[X, Y], proj: PrjOp[X, Y]) = p.testAndSet(() => proj)

    def visit[X, Y, M](p: Proj[X, Y], proj: DPrjOp[X, Y, M], train: Train[M]) = p.testAndSet(() => {
      train.accept(this)
      (x: X) => proj(x, train.get)
    })
    def visit[X, Y, I](p: Proj[X, Y], query: DPrjOp[X, Y, I], index: Index[I]) = p.testAndSet(() => {
      index.accept(this)
      (x: X) => query(x, index.get)
    })
    def visit[X, M](t: Train[M], train: TrnOp[X, M], source: Source[X]) = t.testAndSet(() => {
      source.accept(this)
      train(source.get)
    })
    def visit[X, I](i: Index[I], indexer: IdxOp[X, I], source: Source[X]) = i.testAndSet(() => {
      source.accept(this)
      indexer(source.get)
    })

    // visit pipes   
    def visit[X, Y](s: SourcePipe[X, Y]) = s.testAndSet(() => {
      s.left.accept(this)
      s.right.accept(this)
      s.left.get.map(f =>
        {
          (f._1, s.right.get(f._2))
        })
    })
    def visit[X, L](f: FilterPipe[X, L]) = f.testAndSet(() => {
      f.left.accept(this)
      f.list.accept(this)
      f.left.get.filter(f.filter(f.list.get))
    })
    def visit[X](s: SortPipe[X]) = s.testAndSet(() => {
      s.left.accept(this)
      s.left.get.sortWith(s.right)
    })
    def visit[X](t: TopPipe[X]) = t.testAndSet(() => {
      t.left.accept(this)
      t.left.get.take(t.n)
    })
    // maybe slow and not useful
    def visit[X, Y, C](j: JoinPipe[X, Y, C]) = j.testAndSet(() => {
      j.left.accept(this)
      j.right.accept(this)
      val list: Col[C] = constructListWithAssociatedID(j.left.get.toMap[ID, X], j.right.get.toMap[ID, Y], j.op)
      list
    })

    def visit[X, Y, Z](p: ProjPipe[X, Y, Z]) = p.testAndSet(() => {
      p.left.accept(this)
      p.right.accept(this)
      new Function1[X, Z] { def apply(x: X) = p.right.get(p.left.get(x)) }
    })
  }
}

trait Parallel extends Stage with IDAssociationHelper {
  type Col[X] = ParSeq[FeatureDoc[X]]
  type Idx[X] = X
  type Trn[X] = X
  type Prj[X] = X

  // Index is generated by the pipeline semantics while it is abstract to domain functions

  object ParallelVisitor extends StageVisitor {

    // visit stages
    def visit[P, X](l: Source[X], loader: LoadOp[P, X], path: Dir[P]) = l.testAndSet(() =>
      //path.map( file => (file.toString, loader(file)) )
      path.map(file =>
        {
          val id = file.toString
          (id.substring(id.lastIndexOf('/') + 1, id.length), loader(file))
        }).par)

    def visit[X, Y](p: Proj[X, Y], proj: PrjOp[X, Y]) = p.testAndSet(() => proj)

    def visit[X, Y, M](p: Proj[X, Y], proj: DPrjOp[X, Y, M], train: Train[M]) = p.testAndSet(() => {
      train.accept(this)
      (x: X) => proj(x, train.get)
    })
    def visit[X, Y, I](p: Proj[X, Y], query: DPrjOp[X, Y, I], index: Index[I]) = p.testAndSet(() => {
      index.accept(this)
      (x: X) => query(x, index.get)
    })
    def visit[X, M](t: Train[M], train: TrnOp[X, M], source: Source[X]) = t.testAndSet(() => {
      source.accept(this)
      train(source.get.toList)
    })
    def visit[X, I](i: Index[I], indexer: IdxOp[X, I], source: Source[X]) = i.testAndSet(() => {
      source.accept(this)
      indexer(source.get.toList)
    })

    // visit pipes   
    def visit[X, Y](s: SourcePipe[X, Y]) = s.testAndSet(() => {
      s.left.accept(this)
      s.right.accept(this)
      s.left.get.map(f => (f._1, s.right.get(f._2)))
    })
    def visit[X, L](f: FilterPipe[X, L]) = f.testAndSet(() => {
      f.left.accept(this)
      f.list.accept(this)
      f.left.get.filter(f.filter(f.list.get.toList))
    })
    def visit[X](s: SortPipe[X]) = s.testAndSet(() => {
      s.left.accept(this)
      s.left.get.toList.sortWith(s.right).par
    })
    def visit[X](t: TopPipe[X]) = t.testAndSet(() => {
      t.left.accept(this)
      t.left.get.take(t.n)
    })
    // maybe slow and not useful
    def visit[X, Y, C](j: JoinPipe[X, Y, C]) = j.testAndSet(() => {
      j.left.accept(this)
      j.right.accept(this)
      constructListWithAssociatedID(j.left.get.seq.toMap[ID, X], j.right.get.seq.toMap[ID, Y], j.op).par
      //val r = j.right.get.toMap[ID, Y]
      
      //        		j.left.get.toMap[ID, X].foldLeft[Map[ID, C]](Map.empty)(
      //        					(sofar, pair) => if (r.contains(pair._1))
      //        									(sofar.+[C](pair._1, j.op(pair._2, r.get(pair._1).get)))
      //        									else (sofar)).toList.par
    })
    def visit[X, Y, Z](p: ProjPipe[X, Y, Z]) = p.testAndSet(() => {
      p.left.accept(this)
      p.right.accept(this)
      new Function1[X, Z] { def apply(x: X) = p.right.get(p.left.get(x)) }
    })
  }
}
