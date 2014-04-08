package factorygame.util

import factorygame.util.geom.{P2, P3, P4}

final case class Indexer2D[@specialized T](x: Int, y: Int, v: T) extends P2
final case class Indexer3D[@specialized T](x: Int, y: Int, z: Int, v: T) extends P3
final case class Indexer4D[@specialized T](x: Int, y: Int, z: Int, w: Int, v: T) extends P4
