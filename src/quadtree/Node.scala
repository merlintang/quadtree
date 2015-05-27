package quadtree



import scala.collection.mutable.ArrayBuffer

/**
 * Created by merlin on 5/22/15.
 */
class Node( mx:Double, my:Double, mw:Double, mh:Double, Parent:Node) extends  Serializable  {

  var x=mx

  var y=my

  var w=mw

  var h=mh

  var nw:Node = null

  var ne:Node = null

  var sw:Node = null

  var se:Node = null

  var datapoint:Point=null
  //private val capacity=4

  def this()
  {
    this(0,0,0,0,null)
  }

  def this(mx:Double, my:Double, mw:Double, mh:Double)
  {
    this(mx,my,mw,mh, null)
  }



  import quadtree.NodeType._

  var ntype:NodeType= NodeType.EMPTY

  /**
   * the parent node of this node
   */
  //var Parent:Node=null

  /**
   * those points contained inside this rectangle
   */
  //var points:ArrayBuffer[Point]=null


  /**
   * check this new point inside the boundary or not
   * @param p
   * @return
   */
  def insideBoundary(p:Point):Boolean=
  {
    if(x<p.x&&p.x<w&&y<p.y&&p.y<h)
    {
      true
    }
    false
  }

}
