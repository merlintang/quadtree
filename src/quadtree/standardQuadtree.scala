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
        Util.NODE_Storage match
        {
          case Util.NodeStorage.HashWay=>
            root.NODE_POINTS_HASH.contains(datapoint.hashCode())
          case Util.NodeStorage.VectorWay=>
            root.NODE_POINTS_VECTOR.exists((p:Point)=>p.x == datapoint.x&&p.y==datapoint.y)
        }

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
  def rangeQuery(rectangle:Rectangle,f:(Node, Rectangle)=>Iterator[Point] ): Vector[Point]=
  {
    this.resultSet=null
    this.resultSet=scala.collection.immutable.Vector.empty
    navigate(this.root,rectangle,f)
    this.resultSet
  }

  /**
   * the depth is the maximum depth from root to leaf
   * @return the depth of the quadtree
   */
  def depthQuadtree(): Int =
  {
      val depth=1

    def depthRecursive(pnode:Node, depth:Int):Int=
    {
      pnode.ntype match
      {
        case NodeType.LEAF =>
          depth+1

        case NodeType.POINTER =>

          math.max(
            math.max(depthRecursive(pnode.ne,depth+1),
              depthRecursive(pnode.nw,depth+1)),
            math.max(depthRecursive(pnode.se,depth+1),
              depthRecursive(pnode.sw,depth+1))
          )

        case NodeType.EMPTY=>
          depth
      }

    }

      depthRecursive(this.root,depth)
  }

  /**
   *
   * @return the number of node of quadtree
   */
  def numberOfNodes(): Int =
  {
    val depth=1

    def depthRecursive(pnode:Node, depth:Int):Int=
    {
      pnode.ntype match
      {
        case NodeType.LEAF =>
          depth+4

        case NodeType.POINTER =>
          depthRecursive(pnode.ne,depth+4)+ depthRecursive(pnode.nw,depth+4)+
            depthRecursive(pnode.se,depth+4)+ depthRecursive(pnode.sw,depth+4)

        case NodeType.EMPTY=>
          depth
      }

    }

    depthRecursive(this.root,depth)

  }

  /**
   *
   * @return the number of node of quadtree
   */
  def pointsOfLeaf(): Double =
  {
    val depth=1

    def depthRecursive(pnode:Node):Double=
    {
      pnode.ntype match
      {
        case NodeType.LEAF =>
          pnode.NODE_NUM_POINTS

        case NodeType.POINTER =>
          (depthRecursive(pnode.ne)+ depthRecursive(pnode.nw)+
            depthRecursive(pnode.se)+ depthRecursive(pnode.sw))/4

        case NodeType.EMPTY=>
          1
      }

    }

    depthRecursive(this.root)

  }

  /**
   * println the quadtree structure
   */
  def printTreeStructure(): Unit =
  {

    //val queue:Queue[Node]=new Queue()

    val queue = new scala.collection.mutable.Queue[Node]

    queue+=this.root

    var front=1
    var end=queue.length
    var depth=1;

    while(!queue.isEmpty)
    {

      val pnode=queue.dequeue()

      pnode.ntype match
      {
        case NodeType.LEAF =>
          print(" lnode ")

        case NodeType.POINTER =>
          queue.enqueue(pnode.ne)
          queue.enqueue(pnode.nw)
          queue.enqueue(pnode.se)
          queue.enqueue(pnode.sw)
          printf(" Pnode ")
        case NodeType.EMPTY=>
          printf(" Enode ")
      }

      front=front+1

      if(front>end)
      {
        depth=depth+1
        println("\n--------------------------------------------------")
        front=1
        end=queue.length
      }

    }

    println("depth is "+depth)


  }


  /**
   * navigate the quadtree to find points that meet the function
   * @param pnode
   * @return
   */
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Vector[Point], rets:Iterator[Point]) : Unit=
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
  def navigate (pnode:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Iterator[Point]) : Unit=
  {
    pnode.ntype match
    {
      case NodeType.LEAF =>
        val ret=f(pnode,rectangle)
        //println("!!!!!!!!!!!!!number of nodes here "+pnode.NODE_NUM_POINTS)
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
    if(!this.root.insideBoundary(datapoint)&&this.contain(datapoint))
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
   // val hw = ((Pnode.w-Pnode.x) / 2).toInt;
    //val hh = ((Pnode.h-Pnode.y) / 2).toInt;

    val hw = ((Pnode.w-Pnode.x) / 2);
    val hh = ((Pnode.h-Pnode.y) / 2);

    Pnode.nw=new Node(x,y+hh,x+hw,Pnode.h,Pnode) //north west
    Pnode.ne=new Node(x + hw, y+hh, Pnode.w, Pnode.h,Pnode) //north east
    Pnode.se=new Node(x+hw, y, Pnode.w, y+hh,Pnode) //south east
    Pnode.sw=new Node(x, y, x+hw, y+hh,Pnode) //south west

    Pnode.ntype=NodeType.POINTER

    Util.NODE_Storage match
    {
      case Util.NodeStorage.HashWay=>
        Pnode.NODE_POINTS_HASH.valuesIterator.foreach((elem:Point)=>this.insertPoint(elem,Pnode))
        Pnode.NODE_POINTS_HASH=null

      case Util.NodeStorage.VectorWay=>
        Pnode.NODE_POINTS_VECTOR.foreach((elem:Point)=>this.insertPoint(elem,Pnode))
        Pnode.NODE_POINTS_VECTOR=null
    }

    //set those data points to be null

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
