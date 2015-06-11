package quadtree

/**
 * Created by merlin on 5/22/15.
 */
class Point (var x: Double, var y:Double) extends Serializable {


  def add(other :Point) : Point= {

    this.x=other.x+this.x
    this.y=other.y+this.y
    //this.x=m

    this
  }

  override def toString():String= "( "+x+", "+ y+ " )"

  def copy() :Point ={
    new Point(x,y)
  }

  override def hashCode():Int=
  {

    (this.x*10000+this.y*1000000).toInt

  }
  //def y


}
