package quadtree

import scala.collection.mutable.ArrayBuffer

/**
 * Created by merlin on 5/22/15.
 */
class Node(val x:Double, val y:Double, val w:Double, val h:Double, Parent:Node) extends  Serializable  {

  var nw:Node = null

  var ne:Node = null

  var sw:Node = null

  var se:Node = null

  import quadtree.NodeType._
  var ntype:NodeType= NodeType.EMPTY
  var datapoint:Point=null
  var NODE_NUM_POINTS:Int=0

  private var NODE_POINTS:Vector[Point]=null

  override def  toString():String=
  {
    "Range: " + x + ", " + y + "," +w +","+h +")"
  }

  def this()
  {
    this(0,0,0,0,null)
  }

  def this(mx:Double, my:Double, mw:Double, mh:Double)
  {
    this(mx,my,mw,mh, null)
  }

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
    }else {
      false
    }
  }

  /**
   * add one point into the NODE
   * @param pt
   */
  def addPoint(pt:Point):Unit=
  {
    if(this.NODE_POINTS==null)
    {
        this.NODE_POINTS=Vector(pt)
    }else
    {
        this.NODE_POINTS:+pt
    }

    this.NODE_NUM_POINTS++
  }

}
