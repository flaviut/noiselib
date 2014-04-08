package factorygame.util.geom

import scala.util.hashing.MurmurHash3._

trait P2 extends Product2[Int, Int] {
	def x: Int
	def y: Int

	override def canEqual(that: Any): Boolean = that.isInstanceOf[P2]
	override def _1: Int = x
	override def _2: Int = y

	override def hashCode = {
		var h = mix(0xC936560E, x)
		h = mixLast(h, y)
		finalizeHash(h, 2)
	}
	override def equals(obj: Any): Boolean = obj match {
		case v: P3 => v.x == x && v.y == y
		case _ => false
	}
}

trait P3 extends P2 with Product3[Int, Int, Int] {
	def z: Int

	override def canEqual(that: Any): Boolean = that.isInstanceOf[P3]
	override def _3: Int = z

	override def hashCode = {
		var h = mix(0xC936560E, x)
		h = mix(h, y)
		h = mixLast(h, z)
		finalizeHash(h, 3)
	}
	override def equals(obj: Any): Boolean = obj match {
		case v: P3 => super.equals(obj) && v.z == z
		case _ => false
	}
}

trait P4 extends P3 with Product4[Int, Int, Int, Int] {
	def w: Int

	override def canEqual(that: Any): Boolean = that.isInstanceOf[P4]
	override def _4: Int = w

	override def hashCode = {
		var h = mix(0xC936560E, x)
		h = mix(h, y)
		h = mix(h, z)
		h = mixLast(h, w)
		finalizeHash(h, 4)
	}
	override def equals(obj: Any): Boolean = obj match {
		case v: P4 => super.equals(obj) && v.w == w
		case _ => false
	}
}

case class Point2(x: Int, y: Int) extends P2

case class Point3(x: Int, y: Int, z: Int) extends P3

case class Point4(x: Int, y: Int, z: Int, w: Int) extends P4