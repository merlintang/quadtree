package quadtree

import quadtree.NodeType._

/**
 * Created by merlin on 5/30/15.
 */
object Util {

  val NODE_CPACITY=50000

  object NodeStorage extends Enumeration {

    type NodeStorage=Value
    val VectorWay, HashWay=Value

  }

  val NODE_Storage=Util.NodeStorage.HashWay


}
