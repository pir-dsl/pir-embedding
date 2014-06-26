package edu.uwm.cs.pir.pipeline

import edu.uwm.cs.pir.Base
// definitions for abstract pipeline graph

trait Pipeline extends Base {
  // cache representation for various execution mode e.g. String, X, List[X], RDD[X]
  
  type Col[X] // cache representation of feature collections
  type Idx[X] // cache representation of indexer
  type Trn[X] // cache representation of trainer
  type Prj[X] // cache representation of projector

  
  trait Vertex [C] {
    var cache: Option[C]	= None
    def testAndSet(f: () => C) = cache match { case None 	=> cache = Some (f()) 
	  										   case Some(_) => }
    def get = cache.get
  }
  
  // pipeline operators (methods) available to PIR
  //   Source: 		connect, filter, sort, join, index, query 
  //   SortPipe: 	top
  //   Proj:   		connect 
  
  trait Source[X] extends Vertex[Col[X]] {
	
	def accept(v: PipelineVisitor) = v.visit[X](this) 
	
	def connect[Y]	(p: Proj[X, Y])										= SourcePipe[X, Y](this, p)
	def filter[L] 	(f: FltOp[X, L], l: Source[L])						= FilterPipe[X, L](this, f, l)
	def sort		(order: OdrOp[X])									= SortPipe[X](this, order)
	def join[Y, C]	(right: Source[Y]) (implicit op: CpsOp[X,Y,C]=((x:X,y:Y)=>(x,y)))				= JoinPipe[X, Y, C](this, right, op)
	def index[I]	(f: IdxOp[X, I])									= index_op[X, I](f)(this)
	def query[R,I]	(f: DPrjOp[X, R, I], i :Index[I]) 					= connect[R](query_op(f, i))
	def train[M]	(f: TrnOp[X, M])									= train_op[X, M](f)(this)
	
	// syntactic sugar for connecting source to projectors and dependent projectors 
	def connect[Y]	(f: PrjOp[X, Y])	: SourcePipe[X,Y]				= connect[Y](proj(f))
	def connect[Y,M](f: DPrjOp[X, Y, M], m: Train[M]): SourcePipe[X,Y]	= connect[Y](proj(f,m))
	
	def connect[Y, M](m: Train[M])  									= (f: DPrjOp[X,Y,M]) => connect[Y](proj(f,m))
  }
  case class SourcePipe[In, Out]	(left: Source[In], right: Proj[In, Out])					extends Source[Out] 		{ override def accept(v: PipelineVisitor) { v.visit[In, Out](this)} }
  case class FilterPipe[In, L]		(left: Source[In], filter: FltOp[In, L], list: Source[L])  	extends Source[In]			{ override def accept(v: PipelineVisitor) { v.visit[In, L](this)} }
  case class SortPipe[In]			(left: Source[In], right: OdrOp[In])   						extends Source[In]			{ override def accept(v: PipelineVisitor) { v.visit[In](this)} 
  	def top			(n: Integer)										= TopPipe[In](this, n)
  }
  case class TopPipe[In]			(left: Source[In], n: Integer)								extends Source[In]			{ override def accept(v: PipelineVisitor) { v.visit[In](this)} }
  case class JoinPipe[Left,Right,C]	(left: Source[Left], right: Source[Right], op: CpsOp[Left,Right,C])	
  																								extends Source[C]			{ override def accept(v: PipelineVisitor) { v.visit[Left, Right, C](this)} }

  trait Proj[X, Y] extends Vertex[Prj[PrjOp[X, Y]]] {
   
    def accept(v: PipelineVisitor)  = v.visit[X, Y](this) 
    
	def connect[Z]	(q: Proj[Y, Z])												= ProjPipe(this, q)
	
	// syntactic sugar for chaining projectors and dependent projectors
	def connect[Z]	(f: PrjOp[Y, Z]) : ProjPipe[X,Y,Z]							= connect[Z](proj[Y,Z](f)) 
	def connect[Z,M](f: DPrjOp[Y, Z, M], m: Train[M]) : ProjPipe[X,Y,Z]			= connect[Z](proj[Y,Z,M](f,m))
	
	def connect[Z,M](m: Train[M])												= (f: DPrjOp[Y,Z,M]) => connect[Z](proj[Y,Z,M](f, m))
  }
  case class ProjPipe[In, Middle, Out] (left: Proj[In, Middle], right: Proj[Middle, Out]) extends Proj[In, Out] { override def accept(v: PipelineVisitor) { v.visit[In, Middle, Out](this)} }
	
  trait Train[M] extends Vertex[Trn[M]] {	def accept(v: PipelineVisitor)	= v.visit[M](this) }
  trait Index[I] extends Vertex[Idx[I]] {	def accept(v: PipelineVisitor)	= v.visit[I](this) }
	
  
  // pipeline functions available to PIR but they can be either implicit or be replaced by PIR operators
  // load, proj, train_op, index_op, query_op
  
  implicit def load[P, X] 		(f: LoadOp[P, X]) 								: Dir[P] => Source[X]
  implicit def proj[X, Y] 		(f: PrjOp[X, Y]) 								: Proj[X, Y]
  def proj[X, Y, M] 			(f: DPrjOp[X, Y, M], m : Train[M]) 				: Proj[X, Y]
  
  def proj[X, Y, M] 			(m : Train[M]) 									: DPrjOp[X, Y, M] => Proj[X, Y]
  
  def train_op[X, M] 			(f: TrnOp[X, M])								: Source[X] =>Train[M]
  def index_op[X, I] 			(f: IdxOp[X, I])								: Source[X] => Index[I]
  def query_op[X, Y, I] 		(f: DPrjOp[X, Y, I], i : Index[I])				: Proj[X, Y] 
  
    
  trait PipelineVisitor {
    def visit[X]		(s: Source[X])
    def visit[X, Y]		(p: Proj[X, Y])
    def visit[M]		(t: Train[M])
    def visit[I]		(i: Index[I])
    
    def visit[X, Y] 	(s: SourcePipe[X, Y])
    def visit[X, L] 	(f: FilterPipe[X, L])
    def visit[X]		(s: SortPipe[X])
    def visit[X]		(t: TopPipe[X])
    def visit[X, Y, C]	(j: JoinPipe[X, Y, C])
    def visit[X, Y, Z]	(p: ProjPipe[X, Y, Z])
  }
}
 
// definition for concrete pipeline stages

trait Stage extends Pipeline {
  case class LoadStage[P, X]			(load: LoadOp[P, X], path: Dir[P]) 				extends Source[X] 	
  case class ProjStage[X, Y]			(proj: PrjOp[X, Y]) 							extends Proj[X, Y]  
  case class ProjWithModelStage[X, Y, M](proj: DPrjOp[X,Y,M], model: Train[M]) 			extends Proj[X, Y]
  case class TrainStage[X, M] 			(train: TrnOp[X, M], source: Source[X])			extends Train[M]
  case class IndexStage[X, I]			(indexer: IdxOp[X, I], source: Source[X])		extends Index[I]
  case class ProjWithIndexStage[X, Y, I](query: DPrjOp[X, Y, I], index: Index[I]) 		extends Proj[X, Y]
  
  implicit
  override def load[P, X] 		(f: LoadOp[P, X]) 							= (s: Dir[P]) => LoadStage[P, X](f, s)
  
  implicit 
  override def proj[X, Y] 		(f: PrjOp[X, Y]) 							= ProjStage[X, Y](f)
  override def proj[X, Y, M] 	(f: DPrjOp[X, Y, M], m: Train[M]) 			= ProjWithModelStage[X, Y, M](f, m)

  override def proj[X, Y, M] 	(m : Train[M]) 								= (f: DPrjOp[X, Y, M]) => ProjWithModelStage[X, Y, M](f, m)
  
  override def train_op[X, M]	(f: TrnOp[X, M])  							= (s: Source[X]) => TrainStage[X, M](f, s)
  override def index_op[X, I]	(f: IdxOp[X, I])							= (s: Source[X]) => IndexStage[X, I](f, s)
  override def query_op[X, R, I](f: DPrjOp[X, R, I], i: Index[I])			= ProjWithIndexStage[X, R, I](f, i) 
  
  trait StageVisitor extends PipelineVisitor {
      def visit[P, X] 				(s: Source[X], 		loader:  LoadOp[P, X], 			path: Dir[P])
      def visit[X, Y]				(p: Proj[X, Y], 	proj: 	 PrjOp[X, Y]   )
      def visit[X, Y, M]			(p: Proj[X, Y], 	proj: 	 DPrjOp[X, Y, M], 		train: Train[M])
      def visit[X, Y, I]			(p: Proj[X, Y], 	query: 	 DPrjOp[X, Y, I], 		index: Index[I])
      def visit[X, M]				(t: Train[M], 		trainer: TrnOp[X, M], 			s: Source[X])
      def visit[X, I]				(i: Index[I], 		indexer: IdxOp[X, I], 			s: Source[X])
      
	  def visit[X] 		(s: Source[X]) 	= s match { case LoadStage(loader, path) => visit(s, loader, path)  } 
	  
	  def visit[X, Y]	(p: Proj[X, Y]) = p match { case ProjStage(proj) => visit(p, proj)
	                                   				case ProjWithModelStage(proj, train) => visit(p, proj, train)
	                                   				case ProjWithIndexStage(query, index) => visit(p, query, index)
	    }
	  def visit[M]		(t: Train[M]) 	= t match { case TrainStage(f, s) => visit(t, f, s) }
	  def visit[I]		(i: Index[I])	= i match { case IndexStage(f, s) => visit(i, f, s) }
  } 
  
}





