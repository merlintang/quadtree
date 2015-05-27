package quadtree

import scala.Enumeration

/**
 * Created by merlin on 5/22/15.
 */
object NodeType extends Enumeration {

      type NodeType=Value
      val LEAF, POINTER, EMPTY=Value

}
