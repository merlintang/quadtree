package quadtree

import quadtree._
import scala.collection.immutable.List

/**
  * Created by merlin on 5/22/15.
  */

class standardQuadtree (val root:Node) extends quadtree {


  /**
   * contain the key-value for the given datapoint
   * @param datapoint
   * @return true if this index contain this point
   */
  def contain(datapoint:Point):Boolean=
  {

    def search(datapoint:Point, root:Node):Boolean=
      root match{
      case NodeType.LEAF =>
          if(datapoint.x==root.datapoint.x&&datapoint.y==root.datapoint.y)
            true
          else
            false
      case NodeType.POINTER =>
          search(datapoint, this.getChildNodeForPoint(root,datapoint))
      case NodeType.EMPTY=>
          false
    }

    search(datapoint,this.root)
  }

  /**
   *
   * @param node
   * @param rect
   * @return
   */
  def insidefunction(node:Node, rect:Rectangle): Point=
  {
    if(node.datapoint.x>rect.x&&node.datapoint.x<rect.w
      &&node.datapoint.y>rect.y&&node.datapoint.y<rect.h)
    {
      node.datapoint
    }else
    {
      null
    }
  }

  /**
   * range query to get all points inside the range
   * @param rectangle
   */
  def rangeQuery(rectangle:Rectangle,f:(Node, Rectangle)=>Point ): Unit =
  {
    /**
     * option 2
     */
    val rets = scala.collection.immutable.Vector.empty

    //var inside=new insidefunction()
    navigate(this.root,rectangle,f,rets)

  }


  /**
   * navigate the quadtree to find points that meet the function
   * @param pnode
   * @return
   */
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Point, rets:Vector[Point]) : Unit=
  {
    pnode.ntype match
    {
      case NodeType.LEAF =>
        val ret=f(pnode,rectangle)
        if(ret!=null)
           rets:+ret

      case NodeType.POINTER =>
        if(intersects(rectangle, pnode.ne))
          this.navigate(pnode.ne,rectangle, f,rets)

        if(intersects(rectangle, pnode.nw))
          this.navigate(pnode.nw,rectangle, f,rets)

        if(intersects(rectangle, pnode.se))
          this.navigate(pnode.se,rectangle, f,rets)

        if(intersects(rectangle, pnode.sw))
          this.navigate(pnode.sw,rectangle, f,rets)

      case NodeType.EMPTY=>
    }
    //Vector(new Point)
  }


  /**
   * a rectangle intersect with the boundary of the Node's boundary
   * @param rec
   * @param node
   * @return
   */
   def intersects(rec:Rectangle, node:Node):Boolean=
     {

       !(node.x > rec.x ||
         (node.x + node.w) < rec.w ||
         node.y > rec.y ||
         (node.y + node.h) < rec.h)
     }

  /**
   * insert a point into a Quadtree
   * @param datapoint
   * @return
   */
  def insertPoint(datapoint:Point):Boolean=
  {
    this.insertPoint(datapoint, this.root)
  }

  /**
   * insert a point into a Quadtree
   * @param p : the data point to insert
   * @param parent : the root node to insert
   * @return
   */
   def insertPoint (p:Point, parent:Node): Boolean=
     {
       /**
        * check this new point p is inside the quadtree boundary
        */
      if(!parent.insideBoundary(p))
      {
        return false
      }

       if(parent.NODE_NUM_POINTS<4)
     {
       parent.addPoint(p)
       true
     }else{

        var result=false

        parent.ntype match
        {
          case NodeType.EMPTY=>
            parent.addPoint(p)
            parent.ntype=NodeType.LEAF
            result=true

          case NodeType.LEAF=>
            //this is
            this.spilitLeafNode(parent,p)
            this.insertPoint(p,parent)
            result=true

          case NodeType.POINTER=>
            result=this.insertPoint(p, this.getChildNodeForPoint(parent, p))
        }
        result
      }

    }

  /**
   * change this leaf node into a pointer node
   * remove the data from this node, and insert the new data point into one of the sub-node
   * problem:
   * (1)get all four children?
   * (2)recursive insert into this data point into one of the subchildren
   *
   * @param Pnode
   * @param point
   */
  def spilitLeafNode(Pnode:Node, point:Point): Unit =
  {

    Pnode.ntype=NodeType.POINTER

    //remove the data point from this node into his children
    val tmppoint:Point=Pnode.datapoint.copy()

    Pnode.datapoint=null
    //(a) new children list
    val x = Pnode.x
    val y = Pnode.y;
    val hw = (Pnode.w-Pnode.x) / 2;
    val hh = (Pnode.h-Pnode.y) / 2;

    Pnode.nw=new Node(x,y+hh,x+hw,Pnode.h,Pnode) //north west
    Pnode.ne=new Node(x + hw, y+hh, Pnode.w, Pnode.h,Pnode) //north east
    Pnode.se=new Node(x+hw, y, Pnode.w, y+hh,Pnode) //south east
    Pnode.sw=new Node(x, y, x+hw, y+hh,Pnode) //south west

    this.insertPoint(tmppoint, Pnode)

  }

  /**
   *
   * @param Pnode
   * @param PPoint
   * @return
   */
  def getChildNodeForPoint(Pnode:Node, PPoint:Point): Node=
  {

    val halfx=(Pnode.w-Pnode.x)/2
    val halfy=(Pnode.h-Pnode.y)/2

    if(PPoint.x<Pnode.x+halfx)
    {
      if(PPoint.y<Pnode.y+halfy) Pnode.sw else Pnode.nw
    }else
    {
      if(PPoint.y<Pnode.y+halfy) Pnode.se else Pnode.ne
    }

  }



 }
