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

  var NODE_NUM_POINTS:Int=0
  var NODE_POINTS_VECTOR:Vector[Point]=null
  var NODE_POINTS_HASH:scala.collection.mutable.HashMap[Int, Point]=null
  var NODE_ITERATOR:Iterator[Point]=null

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
    Util.NODE_Storage match
    {
      case Util.NodeStorage.HashWay=>
        this.addPoint2Hash(pt)

      case Util.NodeStorage.VectorWay=>
        this.addPoint2Vector(pt)
    }
  }


  def getIterator():Iterator[Point]=
  {
    Util.NODE_Storage match
    {
      case Util.NodeStorage.HashWay=>
        this.NODE_POINTS_HASH.valuesIterator

      case Util.NodeStorage.VectorWay=>
        this.NODE_POINTS_VECTOR.iterator
    }

  }

  /**
   * use one hashmap to store the datapoints inside one node
   * @param pt
   */
  def addPoint2Hash(pt:Point):Unit=
  {
    try {

      if(this.NODE_POINTS_HASH==null)
      {
       // println("expection before here?")

        this.NODE_POINTS_HASH= new scala.collection.mutable.HashMap()
        this.NODE_POINTS_HASH.put(pt.hashCode(),pt)

        //println("---")

      }
      else
      {
        if(!this.NODE_POINTS_HASH.contains(pt.hashCode()))
        {
          this.NODE_POINTS_HASH.put(pt.hashCode(),pt)
          //pt.hashCode()
        }
      }

  }catch {
      case e: IllegalStateException    => println("illegal state exception");
      case e: OutOfMemoryError         => println("out of memory exception");
      case e: Exception               =>println("unknown error");
  }


    this.NODE_NUM_POINTS=this.NODE_NUM_POINTS+1
  }


  /**
   * add one point into the NODE
   * @param pt
   */
  def addPoint2Vector(pt:Point):Unit=
  {
    if(this.NODE_POINTS_VECTOR==null)
    {
      this.NODE_POINTS_VECTOR=Vector(pt)
    }else
    {
      this.NODE_POINTS_VECTOR=this.NODE_POINTS_VECTOR:+pt
    }

    this.NODE_NUM_POINTS=this.NODE_NUM_POINTS+1

  }

}
