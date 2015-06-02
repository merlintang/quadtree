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
   * rang search
   * @param Index
   * @return
   */
  def test4rangeSearch(Index:standardQuadtree, f:(Node, Rectangle)=>Vector[Point]):Boolean=
  {

    val Rect1=new Rectangle(3,3,30,30)

    val rets=Index.rangeQuery(Rect1,f)

    println("range search result for rectangle query "+Rect1.toString())
    rets.foreach((v:Point)=>println(v.toString()))

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
   * main test function
   * @param args
   */
  def main(args: Array[String]) {

    val rootnode=new Node(-1,-1, 1000, 1000)
    val quadtree=new standardQuadtree(rootnode)

    /******************************************************************/
    println("test the insert operation")

    def GuassianRandom(MinX:Int): Int =
    {
      (util.Random.nextGaussian()%MinX).toInt
    }

    //test4insertPoint(quadtree,util.Random.nextInt,1000,10,50,10,50)

    test4insertPoint(quadtree,GuassianRandom,50,10,50,10,50)

    /******************************************************************/
    println("test the range search")
    def insidefunction(node:Node, rect:Rectangle): Vector[Point]= {
      node.NODE_POINTS.filter((elem:Point)=>(elem.x > rect.x && elem.x < rect.w && elem.y > rect.y && elem.y < rect.h))
    }
    test4rangeSearch(quadtree,insidefunction)

    /******************************************************************/
    test4Depth(quadtree)

  }
    //test quadtree iterator

    //get the iterator

}
