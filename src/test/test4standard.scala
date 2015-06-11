package test

import quadtree._
import scala.collection.mutable.ArrayBuffer


/**
 * Created by merlin on 5/26/15.
 */
object test4standardQuadtree {

  /**
   * add point
   * test the insert correct or not
   * @param Index
   * @return
   */
  def test4insertPoint(Index:standardQuadtree):Boolean=
  {

    val datapoints=new ArrayBuffer[Point]()

    val point1=new Point(2, 2)
    val point2=new Point(4, 6)
    val point3=new Point(60, 70)
    val point4=new Point(55, 6)
    val point5=new Point(3.2, 6)

    datapoints.append(point1)
    datapoints.append(point2)
    datapoints.append(point3)
    datapoints.append(point4)
    datapoints.append(point5)

    /**
     * test the insert operation
     */
    var result=true
    datapoints.foreach(
      (elem:Point)=>(
      if(!Index.insertPoint(elem)) {
        println("this insert fail")
      result=false
    })
    )

    datapoints.foreach(
      (elem:Point)=>(
        if(!Index.contain(elem)) {
          println("error for inserting test for data point:"+elem.toString())
          result=false
        }
        )
    )

    result
    //false
  }

  /**
   * add point
   * test the insert correct or not
   * @param Index
   * @return
   */
  def test4insertPoint(Index:standardQuadtree, randomFunction:(Int)=>Int, NumofPoints:Int, range:Int*):Boolean=
  {

    val datapoints=new ArrayBuffer[Point](NumofPoints)

    //val x=Seq.fill(NumofPoints)(Random.nextInt)

    import util.Random.nextInt
    var i=0
    for( i<-0 until NumofPoints)
    {
      val point=new Point(randomFunction(range(1)-range(0))+range(0), randomFunction(range(3)-range(2))+range(2))
      datapoints.append(point)
    }

    /**
     * test the insert operation
     */
    var result=true
    datapoints.foreach(
      (elem:Point)=>(
        if(!Index.insertPoint(elem)) {
          println("this insert fail")
          result=false
        })
    )

    datapoints.foreach(
      (elem:Point)=>(
        if(!Index.contain(elem)) {
          println("error for inserting test for data point:"+elem.toString())
          result=false
        }
        )
    )

    result
    //false
  }

  /**
   * the linear search approach
   * @param randomFunction
   * @param NumofPoints
   * @param range
   */
  def test4baseline(randomFunction:(Int)=>Int, NumofPoints:Int, rect:Rectangle, range:Int*): Unit =
  {
    val datapoints=new ArrayBuffer[Point](NumofPoints)

    //val x=Seq.fill(NumofPoints)(Random.nextInt)

    import util.Random.nextInt
    var i=0
    for( i<-0 until NumofPoints)
    {
      val point=new Point(randomFunction(range(1)-range(0))+range(0), randomFunction(range(3)-range(2))+range(2))
      datapoints.append(point)
    }

    val b2=System.currentTimeMillis
      datapoints.filter((elem:Point)=>(elem.x > rect.x && elem.x < rect.w && elem.y > rect.y && elem.y < rect.h))

    println("baseline range query time: "+(System.currentTimeMillis-b2) +" ms")
  }

  /**
   * rang search
   * @param Index
   * @return
   */
  def test4rangeSearch(Index:standardQuadtree, f:(Node, Rectangle)=>Iterator[Point]):Boolean=
  {

    val Rect1=new Rectangle(3,3,100,100)

    val rets=Index.rangeQuery(Rect1,f)

    //println("range search result for rectangle query "+Rect1.toString())
    //rets.foreach((v:Point)=>println(v.toString()))

    false
  }

  /**
   *
   * @param Index
   */
  def test4Depth(Index:standardQuadtree): Unit =
  {
    println("depth:"+Index.depthQuadtree())

  }

  /**
   *
   * @param Index
   */
  def test4NumberOfNodes(Index:standardQuadtree): Unit =
  {

    println("number of nodes:"+Index.numberOfNodes())

  }

  /**
   *
   * @param Index
   */
  def test4LeafNodes(Index:standardQuadtree): Unit =
  {

    println("number of point in leaf nodes:"+Index.pointsOfLeaf())

  }


  /**
   * main test function
   * @param args
   */
  def main(args: Array[String]) {

    val rootnode=new Node(-1,-1, 1000000, 1000000)
    val quadtree=new standardQuadtree(rootnode)

    /******************************************************************/
    println("test the insert operation")

    def GuassianRandom(MinX:Int): Int =
    {
      //println(util.Random.nextGaussian())
      math.abs(util.Random.nextGaussian()*10000%MinX).toInt
    }

    // memory info
    val mb = 1024*1024
    val runtime = Runtime.getRuntime
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)


    val b = System.currentTimeMillis;

    test4insertPoint(quadtree,util.Random.nextInt,2000000,2,200,2,200)
    //test4insertPoint(quadtree,GuassianRandom,2000000,2,200,2,200)

    println("build index time: "+(System.currentTimeMillis-b)/1e3+" s")

    println("** Used Memory for index:  " + (runtime.totalMemory - runtime.freeMemory) / mb)

    /******************************************************************/
    println("test the range search")
    def insidefunction(node:Node, rect:Rectangle): Iterator[Point]= {
      node.getIterator().filter((elem:Point)=>(elem.x > rect.x && elem.x < rect.w && elem.y > rect.y && elem.y < rect.h))
    }

    val b2=System.currentTimeMillis

    test4rangeSearch(quadtree,insidefunction)

    println("range query time: "+(System.currentTimeMillis-b2) +" ms")

    /******************************************************************/
    test4Depth(quadtree)

    test4NumberOfNodes(quadtree)

    test4LeafNodes(quadtree)

    quadtree.printTreeStructure()

    /******************************************************************/
    //val Rect1=new Rectangle(3,3,100,100)
    //test4baseline(GuassianRandom,2000000,Rect1,2,200,2,200)

  }
    //test quadtree iterator

    //get the iterator

}
