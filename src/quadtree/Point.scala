package quadtree

/**
 * Created by merlin on 5/22/15.
 */
class Point (var x: Double, var y:Double) extends Serializable {


  def add(other :Point) : Point= {

    //x=x+other.x
    //y=y+other.y

    this.x=other.x+this.x
    this.y=other.y+this.y
    //this.x=m

    this
  }

   def tostring():String= "C"+x+", "+ y+ ")"

  //def y


}
