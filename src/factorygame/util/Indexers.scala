package factorygame.util

import factorygame.util.geom.{P2, P3, P4}

/** Allows for iterating over 2D arrays by both element and index */
final case class Indexer2D[@specialized T](x: Int, y: Int, v: T) extends P2
/** Allows for iterating over 3D arrays by both element and index */
final case class Indexer3D[@specialized T](x: Int, y: Int, z: Int, v: T) extends P3
/** Allows for iterating over 4D arrays by both element and index */
final case class Indexer4D[@specialized T](x: Int, y: Int, z: Int, w: Int, v: T) extends P4
