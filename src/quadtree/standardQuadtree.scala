package quadtree

import quadtree._
import scala.collection.immutable.List

/**
  * Created by merlin on 5/22/15.
  */

class standardQuadtree (val root:Node) extends quadtree {


  var resultSet:Vector[Point]=null
  /**
   * contain the key-value for the given datapoint
   * @param datapoint
   * @return true if this index contain this point
   */
  def contain(datapoint:Point):Boolean=
  {

    def search(datapoint:Point, root:Node):Boolean=
      root.ntype match{
      case NodeType.LEAF =>
        root.NODE_POINTS.exists((p:Point)=>p.x == datapoint.x&&p.y==datapoint.y)
      case NodeType.POINTER =>
        search(datapoint, this.getChildNodeForPoint(root,datapoint))

      case NodeType.EMPTY=>
          false
    }

    search(datapoint,this.root)
  }


  /**
   * range query to get all points inside the range
   * @param rectangle
   */
  def rangeQuery(rectangle:Rectangle,f:(Node, Rectangle)=>Vector[Point] ): Vector[Point]=
  {

    this.resultSet=null
    this.resultSet=scala.collection.immutable.Vector.empty
    navigate(this.root,rectangle,f)
    this.resultSet

  }


  /**
   * navigate the quadtree to find points that meet the function
   * @param pnode
   * @return
   */
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Vector[Point], rets:Vector[Point]) : Unit=
  {
    pnode.ntype match
    {
      case NodeType.LEAF =>
       val ret=f(pnode,rectangle)
        rets++ret

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
   * navigate the quadtree to find points that meet the function
   * @param pnode
   * @return
   */
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Vector[Point]) : Unit=
  {
    pnode.ntype match
    {
      case NodeType.LEAF =>
        val ret=f(pnode,rectangle)
        this.resultSet=this.resultSet++ret

      case NodeType.POINTER =>

        if(intersects(rectangle, pnode.ne))
          this.navigate(pnode.ne,rectangle, f)

        if(intersects(rectangle, pnode.nw))
          this.navigate(pnode.nw,rectangle, f)

        if(intersects(rectangle, pnode.se))
          this.navigate(pnode.se,rectangle, f)

        if(intersects(rectangle, pnode.sw))
          this.navigate(pnode.sw,rectangle, f)

      case NodeType.EMPTY=>

    }
    //Vector(new Point)
  }


  /**
   * a rectangle intersect with the boundary of the Node's boundary
   * return true if intersect or return false
   * @param rec
   * @param node
   * @return
   */
   def intersects(rec:Rectangle, node:Node):Boolean=
     {

       if(node.w < rec.x || node.x > rec.w)
          return false

       if(node.h < rec.y || node.y > rec.h)
         return false

       true
     }

  /**
   * insert a point into a Quadtree
   * @param datapoint
   * @return
   */
  def insertPoint(datapoint:Point):Boolean=
  {
    /**
     * check this new point p is inside the quadtree boundary
     */
    if(!this.root.insideBoundary(datapoint))
    {
      return false
    }

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

       if(parent.NODE_NUM_POINTS<Util.NODE_CPACITY)
     {
       //parent.addPoint(p)
       parent.ntype match {
         case NodeType.EMPTY =>
           parent.addPoint(p)
           parent.ntype = NodeType.LEAF

         case NodeType.LEAF =>
           parent.addPoint(p)
       }
       true
     }else{
        //bigger than the capacity
        var result=false
        parent.ntype match
        {
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

    val x = Pnode.x
    val y = Pnode.y;
    val hw = (Pnode.w-Pnode.x) / 2;
    val hh = (Pnode.h-Pnode.y) / 2;

    Pnode.nw=new Node(x,y+hh,x+hw,Pnode.h,Pnode) //north west
    Pnode.ne=new Node(x + hw, y+hh, Pnode.w, Pnode.h,Pnode) //north east
    Pnode.se=new Node(x+hw, y, Pnode.w, y+hh,Pnode) //south east
    Pnode.sw=new Node(x, y, x+hw, y+hh,Pnode) //south west

    Pnode.ntype=NodeType.POINTER
    Pnode.NODE_POINTS.foreach((elem:Point)=>this.insertPoint(elem,Pnode))

    //set those data points to be null
    Pnode.NODE_POINTS=null

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
