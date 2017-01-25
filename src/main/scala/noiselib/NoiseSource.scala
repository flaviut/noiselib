package noiselib

import noiselib.util.{ParBuffer2D, Buffer2D}

abstract class NoiseSource {
	def gen(x: Double, y: Double): Double

	def gen(x: Double, y: Double, z: Double): Double

	def gen(x: Double, y: Double, z: Double, w: Double): Double

	def fillDoubles(b: Buffer2D[Double]): Unit = b.tabulate(v => gen(v.x, v.y))

	def fillFloats(b: Buffer2D[Float]): Unit = b.tabulate(v => gen(v.x, v.y).toFloat)

	def fillBytes(b: Buffer2D[Byte]): Unit = b.tabulate(v => (gen(v.x, v.y) * Byte.MaxValue).toByte)

	def fillShorts(b: Buffer2D[Short]): Unit = b.tabulate(v => (gen(v.x, v.y) * Short.MaxValue).toShort)

	def fillDoubles(b: ParBuffer2D[Double]): Unit = b.tabulate(v => gen(v.x, v.y))

	def fillFloats(b: ParBuffer2D[Float]): Unit = b.tabulate(v => gen(v.x, v.y).toFloat)

	def fillBytes(b: ParBuffer2D[Byte]): Unit = b.tabulate(v => (gen(v.x, v.y) * Byte.MaxValue).toByte)

	def fillShorts(b: ParBuffer2D[Short]): Unit = b.tabulate(v => (gen(v.x, v.y) * Short.MaxValue).toShort)
}
