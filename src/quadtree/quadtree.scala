package quadtree

/**
 * Created by merlin on 5/22/15.
 * point quadree for the point accessing point
 */
abstract class quadtree extends Serializable  {


  private val dimension=2




  /**
   * insert a point into a Quadtree
   * @param p
   * @return
   */
  def insertPoint (p:Point, parent:Node): Boolean


  /**
   * find all points that meet the function
   * @param p
   * @return
   */
  def navigate (p:Node, rectangle:Rectangle, f:(Node, Rectangle)=>Point, rets:List[Point]): Unit



}
