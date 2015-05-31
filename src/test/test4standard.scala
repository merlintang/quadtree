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

    val datapoints=ArrayBuffer[Point]()

    val point1=new Point(2, 7)
    val point2=new Point(4, 6)
    datapoints.append(point1)
    datapoints.append(point2)

    /**
     * test the insert operation
     */
    var result=true
    for(elem<-datapoints; if result)
    {
      if(!Index.insertPoint(elem)) {
        printf("this insert fail")
        result=false
      }
    }

    //test the contain
    for(elem<-datapoints; if result)
    {
      if(!Index.contain(elem)) {
        printf("check the contain operation")
        result=false
      }
    }

    result
    //false
  }

  /**
   * rang search
   * @param Index
   * @return
   */
  def test4rangeSearch(Index:quadtree):Boolean=
  {

    false
  }

  /**
   * main test function
   * @param args
   */
  def main(args: Array[String]) {

    val rootnode=new Node(0,0, 100, 100)
    val quadtree=new standardQuadtree(rootnode)

    test4insertPoint(quadtree)


  }
    //test quadtree iterator

    //get the iterator

}
