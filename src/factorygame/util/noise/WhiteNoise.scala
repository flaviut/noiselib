package factorygame.util
package noise

import factorygame.util.PositionalRandom
import spire.syntax.literals.radix.radix
import spire.syntax.cfor._
import math.abs

/**
 * Creates non-coherent random noise
 */
abstract class WhiteNoise(seed: Int = 0) extends NoiseSource {
	private val rand = new PositionalRandom(seed)

	def gen(x: Double, y: Double): Double

	def gen(x: Double, y: Double, z: Double): Double

	def gen(x: Double, y: Double, z: Double, w: Double): Double

	protected def get(x: Int, y: Int): Double = norm(rand.get(x, y))

	protected def get(x: Int, y: Int, z: Int): Double = norm(rand.get(x, y, z))

	protected def get(x: Int, y: Int, z: Int, w: Int): Double = norm(rand.get(x, y, z, w))

	protected def norm(v: Int): Double = v.toDouble / Int.MaxValue
}

/**
 * Performs smooth transitions between points
 */
trait LinearInterp extends WhiteNoise {
	override def gen(x: Double, y: Double) = {
		val (x1, x2) = (floor(x), floor(x) + 1)
		val (y1, y2) = (floor(y), floor(y) + 1)

		lerp(x, y,
			x1, y1,
			x2, y2,
			Array(
				get(x1, y1),
				get(x1, y2),
				get(x2, y1),
				get(x2, y2)
			))
	}

	override def gen(x: Double, y: Double, z: Double) = {
		val (x1, x2) = (floor(x), floor(x) + 1)
		val (y1, y2) = (floor(y), floor(y) + 1)
		val (z1, z2) = (floor(z), floor(z) + 1)

		lerp(x, y, z,
			x1, y1, z1,
			x2, y2, z2,
			Array(
				get(x1, y1, z1),
				get(x1, y1, z2),
				get(x1, y2, z1),
				get(x1, y2, z2),
				get(x2, y1, z1),
				get(x2, y1, z2),
				get(x2, y2, z1),
				get(x2, y2, z2)
			))
	}

	override def gen(x: Double, y: Double, z: Double, w: Double) = {
		// Yes, the code is ugly. But manual unrolling makes a huge
		// performance difference
		val (x1, x2) = (floor(x), floor(x) + 1)
		val (y1, y2) = (floor(y), floor(y) + 1)
		val (z1, z2) = (floor(z), floor(z) + 1)
		val (w1, w2) = (floor(w), floor(w) + 1)

		lerp(x, y, z, w,
			x1, y1, z1, w1,
			x2, y2, z2, w2,
			Array(
				get(x1, y1, z1, w1),
				get(x1, y1, z1, w2),
				get(x1, y1, z2, w1),
				get(x1, y1, z2, w2),
				get(x1, y2, z1, w1),
				get(x1, y2, z1, w2),
				get(x1, y2, z2, w1),
				get(x1, y2, z2, w2),
				get(x2, y1, z1, w1),
				get(x2, y1, z1, w2),
				get(x2, y1, z2, w1),
				get(x2, y1, z2, w2),
				get(x2, y2, z1, w1),
				get(x2, y2, z1, w2),
				get(x2, y2, z2, w1),
				get(x2, y2, z2, w2)
			))
	}

	@inline protected def interp(a: Double, b: Double, x: Double) = a * (1 - x) + b * x


	@inline private def lerp(x: Double,
	                         x1: Int, x2: Int,
	                         v1: Double, v2: Double): Double = {
		interp(v1, v2, abs(x1 - x))
	}

	@inline private def lerp(x: Double, y: Double,
	                         x1: Int, y1: Int,
	                         x2: Int, y2: Int,
	                         vxx: Array[Double]): Double = {
		val v0 = lerp(x, x1, x2, vxx(x2"00"), vxx(x2"10"))
		val v1 = lerp(x, x1, x2, vxx(x2"01"), vxx(x2"11"))

		lerp(y, y1, y2, v0, v1)
	}

	@inline private def lerp(x: Double, y: Double, z: Double,
	                         x1: Int, y1: Int, z1: Int,
	                         x2: Int, y2: Int, z2: Int,
	                         vxxx: Array[Double]): Double = {
		val vxx = new Array[Double](x2"11" + 1)
		cforRange(x2"000" to x2"011")(v =>
			vxx(v) = lerp(x, x1, x2, vxxx(v), vxxx(v | x2"100")))

		lerp(y, z, y1, z1, y2, z2, vxx)
	}

	@inline private def lerp(x: Double, y: Double,
	                         z: Double, w: Double,
	                         x1: Int, y1: Int, z1: Int, w1: Int,
	                         x2: Int, y2: Int, z2: Int, w2: Int,
	                         vxxxx: Array[Double]): Double = {
		val vxxx = new Array[Double](x2"111" + 1)
		cforRange(x2"0000" to x2"0111")(v =>
			vxxx(v) = lerp(x, x1, x2, vxxxx(v), vxxxx(v | x2"1000")))

		lerp(y, z, w, y1, z1, w1, y2, z2, w2, vxxx)
	}
}

/**
 * Creates abrupt transitions between points
 */
trait NearestNeighborInterp extends WhiteNoise {
	def gen(x: Double, y: Double): Double = get(x.toInt, y.toInt)

	def gen(x: Double, y: Double, z: Double): Double = get(x.toInt, y.toInt, z.toInt)

	def gen(x: Double, y: Double, z: Double, w: Double): Double = get(x.toInt, y.toInt, z.toInt, w.toInt)
}