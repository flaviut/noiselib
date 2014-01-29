package factorygame.util
package noise

import factorygame.util.PositionalRandom
import spire.implicits._
import spire.syntax.literals.radix.radix
import factorygame.util.noise.LinearInterp.UglyLinearInterp

/**
 * Creates non-coherent random noise
 * Uses Nearest-Neighbor interpolation by default
 */
class WhiteNoise(seed: Int = 0) extends NoiseSource {
	private val rand = new PositionalRandom(seed)

	private implicit def long2Int(v: Long) = v.toInt

	def gen(x: Double, y: Double): Double = get(x.round, y.round)
	def gen(x: Double, y: Double, z: Double): Double = get(x.round, y.round, z.round)
	def gen(x: Double, y: Double, z: Double, w: Double): Double = get(x.round, y.round, z.round, w.round)

	protected def get(x: Int, y: Int): Double = norm(rand.get(x, y))
	protected def get(x: Int, y: Int, z: Int): Double = norm(rand.get(x, y, z))
	protected def get(x: Int, y: Int, z: Int, w: Int): Double = norm(rand.get(x, y, z, w))

	protected def norm(v: Int): Double = v.toDouble / Int.MaxValue
}

trait LinearInterp extends WhiteNoise with UglyLinearInterp

object LinearInterp {

	/* Benchmarks for a 100x100 4d grid (gen(x,y,0,0))
   * 0% Scenario{vm=java, trial=0, benchmark=NiceInterp} 161784579.22 ns; σ=534494.67 ns @ 3 trials
   * 50% Scenario{vm=java, trial=0, benchmark=UglyInterp} 249013925.40 ns; σ=1014804.06 ns @ 3 trials
   *
   * benchmark  ms linear runtime
   * UglyInterp 162 ===================
   * NiceInterp 249 ==============================
   */

	/**
	 * Clean implementation, but 2/3 slower than ugly interp
	 */
	protected trait NiceLinearInterp extends WhiteNoise {
		override def gen(x: Double, y: Double) = {
			val (x1, x2) = (floor(x), floor(x) + 1)
			val (y1, y2) = (floor(y), floor(y) + 1)

			lerp(x, y,
				x1, y1,
				x2, y2,
				Array(
					super.get(x1, y1),
					super.get(x1, y2),
					super.get(x2, y1),
					super.get(x2, y2)
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
					super.get(x1, y1, z1),
					super.get(x1, y1, z2),
					super.get(x1, y2, z1),
					super.get(x1, y2, z2),
					super.get(x2, y1, z1),
					super.get(x2, y1, z2),
					super.get(x2, y2, z1),
					super.get(x2, y2, z2)
				))
		}

		override def gen(x: Double, y: Double, z: Double, w: Double) = {
			val (x1, x2) = (floor(x), floor(x) + 1)
			val (y1, y2) = (floor(y), floor(y) + 1)
			val (z1, z2) = (floor(z), floor(z) + 1)
			val (w1, w2) = (floor(z), floor(z) + 1)

			lerp(x, y, z, w,
				x1, y1, z1, w1,
				x2, y2, z2, w2,
				Array(
					super.get(x1, y1, z1, w1),
					super.get(x1, y1, z1, w2),
					super.get(x1, y1, z2, w1),
					super.get(x1, y1, z2, w2),
					super.get(x1, y2, z1, w1),
					super.get(x1, y2, z1, w2),
					super.get(x1, y2, z2, w1),
					super.get(x1, y2, z2, w2),
					super.get(x2, y1, z1, w1),
					super.get(x2, y1, z1, w2),
					super.get(x2, y1, z2, w1),
					super.get(x2, y1, z2, w2),
					super.get(x2, y2, z1, w1),
					super.get(x2, y2, z1, w2),
					super.get(x2, y2, z2, w1),
					super.get(x2, y2, z2, w2)
				))
		}

		// Adapted from https://gist.github.com/begla/1019993, added 4d, modularized, and switched to arrays
		@inline private def lerp(x: Double,
		                         x1: Int, x2: Int,
		                         v1: Double, v2: Double): Double = {
			((x2 - x) / (x2 - x1)) * v1 + ((x - x1) / (x2 - x1)) * v2
		}

		@inline private def lerp(x: Double, y: Double,
		                         x1: Int, y1: Int,
		                         x2: Int, y2: Int,
		                         vxx: Array[Double]): Double = {
			require(vxx.size == 2 ** 2)

			val v0 = lerp(x, x1, x2, vxx(x2"00"), vxx(x2"10"))
			val v1 = lerp(x, x1, x2, vxx(x2"01"), vxx(x2"11"))

			lerp(y, y1, y2, v0, v1)
		}

		@inline private def lerp(x: Double, y: Double, z: Double,
		                         x1: Int, y1: Int,
		                         z1: Int, x2: Int,
		                         y2: Int, z2: Int,
		                         vxxx: Array[Double]): Double = {
			require(vxxx.size == 2 ** 3)
			val vxx = new Array[Double](x2"11" + 1)
			for (v <- x2"000" to x2"011")
				vxx(v) = lerp(x, x1, x2, vxxx(v), vxxx(v | x2"100"))

			lerp(y, z, y1, z1, y2, z2, vxx)
		}

		@inline private def lerp(x: Double, y: Double,
		                         z: Double, w: Double,
		                         x1: Int, y1: Int,
		                         z1: Int, w1: Int,
		                         x2: Int, y2: Int,
		                         z2: Int, w2: Int,
		                         vxxxx: Array[Double]): Double = {
			require(vxxxx.size == 2 ** 4)
			val vxxx = new Array[Double](x2"111" + 1)
			for (v <- x2"0000" to x2"0111")
				vxxx(v) = lerp(x, x1, x2, vxxxx(v), vxxxx(v | x2"1000"))

			lerp(y, z, w, y1, z1, w1, y2, z2, w2, vxxx)
		}
	}

	/**
	 * Ugly, but a bit faster and stabilized on Hotspot faster
	 */
	protected trait UglyLinearInterp extends WhiteNoise {
		override def gen(x: Double, y: Double) = {
			val (x1, x2) = (floor(x), floor(x) + 1)
			val (y1, y2) = (floor(y), floor(y) + 1)
			val (v00, v01) = (super.get(x1, y1), super.get(x1, y2))
			val (v10, v11) = (super.get(x2, y1), super.get(x2, y2))

			lerp(x, y, x1, y1, x2, y2, v00, v01, v10, v11)
		}

		override def gen(x: Double, y: Double, z: Double) = {
			val (x1, x2) = (floor(x), floor(x) + 1)
			val (y1, y2) = (floor(y), floor(y) + 1)
			val (z1, z2) = (floor(z), floor(z) + 1)
			val (v000, v001) = (super.get(x1, y1, z1), super.get(x1, y1, z2))
			val (v010, v011) = (super.get(x1, y2, z1), super.get(x1, y2, z2))
			val (v100, v101) = (super.get(x2, y1, z1), super.get(x2, y1, z2))
			val (v110, v111) = (super.get(x2, y2, z1), super.get(x2, y2, z2))

			lerp(x, y, z,
				x1, y1, z1, x2, y2, z2,
				v000, v001, v010, v011,
				v100, v101, v110, v111)
		}

		override def gen(x: Double, y: Double, z: Double, w: Double) = {
			val (x1, x2) = (floor(x), floor(x) + 1)
			val (y1, y2) = (floor(y), floor(y) + 1)
			val (z1, z2) = (floor(z), floor(z) + 1)
			val (w1, w2) = (floor(z), floor(z) + 1)
			val (v0000, v0001) = (super.get(x1, y1, z1, w1), super.get(x1, y1, z1, w2))
			val (v0010, v0011) = (super.get(x1, y1, z2, w1), super.get(x1, y1, z2, w2))
			val (v0100, v0101) = (super.get(x1, y2, z1, w1), super.get(x1, y2, z1, w2))
			val (v0110, v0111) = (super.get(x1, y2, z2, w1), super.get(x1, y2, z2, w2))
			val (v1000, v1001) = (super.get(x2, y1, z1, w1), super.get(x2, y1, z1, w2))
			val (v1010, v1011) = (super.get(x2, y1, z2, w1), super.get(x2, y1, z2, w2))
			val (v1100, v1101) = (super.get(x2, y2, z1, w1), super.get(x2, y2, z1, w2))
			val (v1110, v1111) = (super.get(x2, y2, z2, w1), super.get(x2, y2, z2, w2))

			lerp(x, y, z, w,
				x1, y1, z1, w1,
				x2, y2, z2, w2,
				v0000, v0001, v0010, v0011,
				v0100, v0101, v0110, v0111,
				v1000, v1001, v1010, v1011,
				v1100, v1101, v1110, v1111)
		}

		// Adapted from https://gist.github.com/begla/1019993, added 4d and made variable ordering better
		private def lerp(x: Double, x1: Int, x2: Int, v1: Double, v2: Double): Double = {
			((x2 - x) / (x2 - x1)) * v1 + ((x - x1) / (x2 - x1)) * v2
		}

		private def lerp(x: Double, y: Double, x1: Int, y1: Int, x2: Int, y2: Int,
		                 v00: Double, v01: Double, v10: Double, v11: Double): Double = {
			val v0 = lerp(x, x1, x2, v00, v10)
			val v1 = lerp(x, x1, x2, v01, v11)
			lerp(y, y1, y2, v0, v1)
		}

		private def lerp(x: Double, y: Double, z: Double,
		                 x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int,
		                 v000: Double, v001: Double, v010: Double, v011: Double,
		                 v100: Double, v101: Double, v110: Double, v111: Double): Double = {
			val v00 = lerp(x, x1, x2, v000, v100)
			val v10 = lerp(x, x1, x2, v010, v110)
			val v01 = lerp(x, x1, x2, v001, v101)
			val v11 = lerp(x, x1, x2, v011, v111)
			val v0 = lerp(y, y1, y2, v00, v01)
			val v1 = lerp(y, y1, y2, v10, v11)

			lerp(z, z1, z2, v0, v1)
		}

		private def lerp(x: Double, y: Double, z: Double, w: Double,
		                 x1: Int, y1: Int, z1: Int, w1: Int, x2: Int, y2: Int, z2: Int, w2: Int,
		                 v0000: Double, v0001: Double, v0010: Double, v0011: Double,
		                 v0100: Double, v0101: Double, v0110: Double, v0111: Double,
		                 v1000: Double, v1001: Double, v1010: Double, v1011: Double,
		                 v1100: Double, v1101: Double, v1110: Double, v1111: Double): Double = {
			val v000 = lerp(x, x1, x2, v0000, v1000)
			val v001 = lerp(x, x1, x2, v0001, v1001)
			val v010 = lerp(x, x1, x2, v0010, v1010)
			val v011 = lerp(x, x1, x2, v0011, v1011)
			val v100 = lerp(x, x1, x2, v0100, v1100)
			val v101 = lerp(x, x1, x2, v0101, v1101)
			val v110 = lerp(x, x1, x2, v0110, v1110)
			val v111 = lerp(x, x1, x2, v0111, v1111)
			val v00 = lerp(y, y1, y2, v000, v100)
			val v01 = lerp(y, y1, y2, v001, v101)
			val v10 = lerp(y, y1, y2, v010, v110)
			val v11 = lerp(y, y1, y2, v011, v111)
			val v0 = lerp(z, z1, z2, v00, v10)
			val v1 = lerp(z, z1, z2, v01, v11)

			lerp(w, w1, w2, v0, v1)
		}
	}

}