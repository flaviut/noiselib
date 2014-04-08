package factorygame.util.geom

import spire.math._
import spire.implicits._

case class Vec2(x: Double, y: Double) {
	def +(v: Vec2) = Vec2(x + v.x, y + v.y)
	def -(v: Vec2) = Vec2(x - v.x, y - v.y)
	def *(v: Double) = Vec2(x * v, y * v)

	def dot(v: Vec2) = (x * v.x) + (y * v.y)
	def ⋅(v: Vec2) = dot(v)

	def angle = atan2(y, x)

	def mag = sqrt(magPow2)
	def magPow2 = (x * x) + (y * y)
}

case class Vec3(x: Double, y: Double, z: Double) {
	def +(v: Vec3) = Vec3(x + v.x, y + v.y, z + v.z)
	def -(v: Vec3) = Vec3(x - v.x, y - v.y, z - v.z)
	def *(v: Double) = Vec3(x * v, y * v, z * v)

	def dot(v: Vec3) = (x * v.x) + (y * v.y) + (z * v.z)
	def ⋅(v: Vec3) = dot(v)

	def mag = sqrt(magPow2)
	def magPow2 = (x * x) + (y * y) + (z * z)
}

case class Vec4(x: Double, y: Double, z: Double, w: Double) {
	def +(v: Vec4) = Vec4(x + v.x, y + v.y, z + v.z, w + v.w)
	def -(v: Vec4) = Vec4(x - v.x, y - v.y, z - v.z, w - v.w)
	def *(v: Double) = Vec4(x * v, y * v, z * v, w * v)

	def dot(v: Vec4) = (x * v.x) + (y * v.y) + (z * v.z) + (w * v.w)
	def ⋅(v: Vec4) = dot(v)

	def mag = sqrt(magPow2)
	def magPow2 = (x * x) + (y * y) + (z * z) + (w * w)
}