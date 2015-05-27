package quadtree

/**
 * Created by merlin on 5/25/15.
 */

class Rectangle (val x:Double, val y:Double, val w:Double, val h:Double) extends Serializable
{

  def this()
  {
      this(0,0,0,0)
  }

  def containsPoint(node:Node):Boolean=
  {


    false
  }

  def intersectsRectangle(r:Rectangle):Boolean=
  {

    false
  }

}