package quadtree

import scala.collection.immutable.List

/**
  * Created by merlin on 5/22/15.
 * follow the java implementation in the
 * https://github.com/varunpant/Quadtree/blob/master/src/main/java/QuadTree.java
  */

class standardImple (val root:Node) extends quadtree {


  /**
   * contain the key-value for the given datapoint
   * @param datapoint
   * @return true if this index contain this point
   */
  def contain(datapoint:Point):Boolean=
  {

    def search(datapoint:Point, root:Node):Boolean={
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
   * range query to get all points inside the range
   * @param rectangle
   */
def rangeQuery(rectangle:Rectangle): Unit =
{

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
   * option 1
   */
   //import scala.collection.mutable.ListBuffer
   //val rets=new ListBuffer[Int]()

  /**
   * option 2
   */
  val rets:List[Point] = Nil

  this.navigate(this.root,rectangle,insidefunction,rets)

}


  /**
   * navigate the quadtree to find points that meet the function
   * @param pnode
   * @return
   */
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Point, rets:List[Point]) : Unit=
  {
    pnode.ntype match
    {
      case NodeType.LEAF =>
        val ret=f(pnode,rectangle)
        if(ret!=null)
           rets+:ret

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
   * @param p
   * @return
   */
   def insertPoint (p:Point, parent:Node): Boolean=
     {
       /**
        * check this new point p is inside the quadtree boundary
        */
      if(!parent.insideBoundary(p))
      {
        false
      }else{

        import quadtree.NodeType._
        var result=false;
        parent.ntype match
        {
          case NodeType.EMPTY =>
            parent.datapoint=p
            result=true

          case NodeType.LEAF=>
            //this is
            if(p.x==parent.datapoint.x&&p.y==parent.datapoint.y)
            {
              result=false
            }
              else
            {
              //get new children list
                this.spilitLeafNode(parent,p)
              // and insert the data point into one of the leaf node
              //problem: this bring overhead to reuse the data
                this.insertPoint(p,parent)
            }
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
    val tmppoint=Pnode.datapoint.clone()
    Pnode.datapoint=null

    //(a) new children list

    val x = Pnode.x
    val y = Pnode.y;
    val hw = Pnode.w / 2;
    val hh = Pnode.h / 2;

    Pnode.nw=new Node(x,y,hw,hh,Pnode)
    Pnode.ne=new Node(x + hw, y, hw, hh,Pnode)
    Pnode.se=new Node(x, y + hh, hw, hh,Pnode)
    Pnode.sw=new Node(x + hw, y + hh, hw, hh,Pnode)

  }

  /**
   *
   * @param Parent
   * @param p
   * @return
   */
  def getChildNodeForPoint(Parent:Node, p:Point): Node=
  {
    val mx=Parent.x+Parent.w/2
    val my=Parent.y+Parent.h/2

    if(p.x<mx)
    {
      if(p.y<my) Parent.nw else Parent.sw
    }else
    {
      if(p.y<my) Parent.ne else Parent.se
    }

  }



 }
