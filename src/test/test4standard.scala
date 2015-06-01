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
   * rang search
   * @param Index
   * @return
   */
  def test4rangeSearch(Index:standardQuadtree, f:(Node, Rectangle)=>Vector[Point]):Boolean=
  {

    val Rect1=new Rectangle(3,3,70,10)

    val rets=Index.rangeQuery(Rect1,f)

    println("range search result for rectangle query "+Rect1.toString())
    rets.foreach((v:Point)=>println(v.toString()))

    false
  }

  /**
   * main test function
   * @param args
   */
  def main(args: Array[String]) {

    val rootnode=new Node(0,0, 100, 100)
    val quadtree=new standardQuadtree(rootnode)

    println("test the insert operation")
    test4insertPoint(quadtree)


    println("test the range search")

    def insidefunction(node:Node, rect:Rectangle): Vector[Point]= {
      node.NODE_POINTS.filter((elem:Point)=>(elem.x > rect.x && elem.x < rect.w && elem.y > rect.y && elem.y < rect.h))
    }
    test4rangeSearch(quadtree,insidefunction)

  }
    //test quadtree iterator

    //get the iterator

}
