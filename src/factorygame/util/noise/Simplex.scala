package factorygame.util.noise

import scala.math._
import scala.util.Random
import scala.language.postfixOps


/**
 * A speed-improved simplex noise algorithm for 2D and 3D in Scala.
 *
 * Created and placed in the public domain by Stefan Gustavson (stegu@itn.liu.se).
 * Optimisations by Peter Eastman (peastman@drizzle.stanford.edu).
 * Better rank ordering method by Stefan Gustavson in 2012.
 *
 * Translated into Scala from Java
 */
class Simplex(val seed: Int = 0) extends NoiseSource {
	/*
	 * Benchmark of 1000x1000 each trial
	 *
	 *       benchmark   ms linear runtime
	 *  Scala2DSimplex 50.2 =========================
	 *  Scala3DSimplex 55.5 ============================
	 *   Java2DSimplex 40.9 ====================
	 *   Java3DSimplex 59.5 ==============================
	 *
	 */

	import Simplex._

	private val perm = {
		val r = new Random(seed)
		val arr = r.shuffle((0 until 256).toIterator).toArray.map(_ toShort)
		arr ++ arr
	}

	private val permMod12 = perm.map(_ % 12 toShort)

	def gen(x: Double, y: Double) = {
		// Hairy factor for 2D
		val s = (x + y) * F2
		val i = floor(x + s)
		val j = floor(y + s)
		val t = (i + j) * G2

		// Unskew the cell origin back to (x,y) space
		// The x,y distances from the cell origin
		val x0 = x - (i - t)
		val y0 = y - (j - t)

		/*
		 * For the 2D case, the simplex shape is an equilateral triangle.
		 * Determine which simplex we are in.
		 * Offsets for second (middle) corner of simplex in (i,j) coords
		 *
		 * A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
		 * a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
		 * c = (3-sqrt(3))/6
		 */
		val (i1, j1) =
			if (x0 > y0)
				(1, 0) // lower triangle, XY order: (0,0)->(1,0)->(1,1)
			else
				(0, 1) // upper triangle, YX order: (0,0)->(0,1)->(1,1)

		// Offsets for middle corner in (x,y) unskewed coords
		val x1 = x0 - i1 + G2
		val y1 = y0 - j1 + G2
		// Offsets for last corner in (x,y) unskewed coords
		val x2 = x0 - 1.0 + 2.0 * G2
		val y2 = y0 - 1.0 + 2.0 * G2

		// Work out the hashed gradient indices of the three simplex corners
		val ii = i & 255
		val jj = j & 255

		var n0 = 0.0
		var n1 = 0.0
		var n2 = 0.0

		// Calculate the contribution from the three corners
		var t0 = 0.5 - x0 * x0 - y0 * y0
		if (t0 >= 0) {
			val gi0 = permMod12(ii + perm(jj))
			t0 *= t0
			// (x,y) of grad3 used for 2D gradient
			n0 = t0 * t0 * dot(grad3(gi0), x0, y0)
		}

		var t1 = 0.5 - x1 * x1 - y1 * y1
		if (t1 >= 0) {
			val gi1 = permMod12(ii + i1 + perm(jj + j1))
			t1 *= t1
			n1 = t1 * t1 * dot(grad3(gi1), x1, y1)
		}

		var t2 = 0.5 - x2 * x2 - y2 * y2
		if (t2 >= 0) {
			val gi2 = permMod12(ii + 1 + perm(jj + 1))
			t2 *= t2
			n2 = t2 * t2 * dot(grad3(gi2), x2, y2)
		}

		70.0 * (n0 + n1 + n2)
	}

	def gen(x: Double, y: Double, z: Double) = {
		// Skew the input space to determine which simplex cell we're in
		val s = (x + y + z) * F3
		val i = floor(x + s)
		val j = floor(y + s)
		val k = floor(z + s)
		val t = (i + j + k) * G3

		// Unskew the cell origin back to (x,y,z) space
		// The x,y,z distances from the cell origin
		val x0 = x - (i - t)
		val y0 = y - (j - t)
		val z0 = z - (k - t)

		/*
		 * For the 3D case, the simplex shape is a slightly irregular tetrahedron.
		 *
		 * Determine which simplex we are in.
		 * Offsets for second and third corner of simplex in (i,j,k) coords
     *
		 * A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
		 * a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
		 * a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
		 * c = 1/6.
		 */
		var (i1, j1) = (0, 0)
		var (k1, i2) = (0, 0)
		var (j2, k2) = (0, 0)

		if (x0 >= y0) {
			if (y0 >= z0) {
				// X Y Z order
				i1 = 1
				j1 = 0
				k1 = 0
				i2 = 1
				j2 = 1
				k2 = 0
			} else if (x0 >= z0) {
				// X Z Y order
				i1 = 1
				j1 = 0
				k1 = 0
				i2 = 1
				j2 = 0
				k2 = 1
			} else {
				// Z X Y order
				i1 = 0
				j1 = 0
				k1 = 1
				i2 = 1
				j2 = 0
				k2 = 1
			}
		} else {
			// x0<y0
			if (y0 < z0) {
				// Z Y X order
				i1 = 0
				j1 = 0
				k1 = 1
				i2 = 0
				j2 = 1
				k2 = 1
			} else if (x0 < z0) {
				// Y Z X order
				i1 = 0
				j1 = 1
				k1 = 0
				i2 = 0
				j2 = 1
				k2 = 1
			} else {
				// Y X Z order
				i1 = 0
				j1 = 1
				k1 = 0
				i2 = 1
				j2 = 1
				k2 = 0
			}
		}

		// Offsets for second corner in (x,y,z) coords
		val x1 = x0 - i1 + G3
		val y1 = y0 - j1 + G3
		val z1 = z0 - k1 + G3
		// Offsets for third corner in (x,y,z) coords
		val x2 = x0 - i2 + 2.0 * G3
		val y2 = y0 - j2 + 2.0 * G3
		val z2 = z0 - k2 + 2.0 * G3
		// Offsets for last corner in (x,y,z) coords
		val x3 = x0 - 1.0 + 3.0 * G3
		val y3 = y0 - 1.0 + 3.0 * G3
		val z3 = z0 - 1.0 + 3.0 * G3

		// Work out the hashed gradient indices of the four simplex corners
		val ii = i & 255
		val jj = j & 255
		val kk = k & 255

		var (n0, n1) = (0.0, 0.0)
		var (n2, n3) = (0.0, 0.0)

		var t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0
		if (t0 >= 0) {
			val gi0: Int = permMod12(ii + perm(jj + perm(kk)))
			t0 *= t0
			n0 = t0 * t0 * dot(grad3(gi0), x0, y0, z0)
		}

		var t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1
		if (t1 >= 0) {
			val gi1: Int = permMod12(ii + i1 + perm(jj + j1 + perm(kk + k1)))
			t1 *= t1
			n1 = t1 * t1 * dot(grad3(gi1), x1, y1, z1)
		}

		var t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2
		if (t2 >= 0) {
			val gi2: Int = permMod12(ii + i2 + perm(jj + j2 + perm(kk + k2)))
			t2 *= t2
			n2 = t2 * t2 * dot(grad3(gi2), x2, y2, z2)
		}

		var t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3
		if (t3 >= 0) {
			val gi3: Int = permMod12(ii + 1 + perm(jj + 1 + perm(kk + 1)))
			t3 *= t3
			n3 = t3 * t3 * dot(grad3(gi3), x3, y3, z3)
		}

		// Add contributions from each corner to get the final noise value.
		// The result is scaled to stay just inside [-1,1]
		32.0 * (n0 + n1 + n2 + n3)
	}

	def gen(x: Double, y: Double, z: Double, w: Double): Double = {
		// Skew the input space to determine which simplex cell we're in
		val s = (x + y + z + w) * F4
		val i = floor(x + s)
		val j = floor(y + s)
		val k = floor(z + s)
		val l = floor(w + s)
		val t = (i + j + k + l) * G4

		// Unskew the cell origin back to (x,y,z,w) space
		// The x,y,z,w distances from the cell origin
		val x0 = x - (i - t)
		val y0 = y - (j - t)
		val z0 = z - (k - t)
		val w0 = w - (l - t)


		/*
		 * For the 4D case, the simplex is a 4D shape I won't even try to describe.
		 * To find out which of the 24 possible simplices we're in, we need to
		 * determine the magnitude ordering of x0, y0, z0 and w0.
		 * Six pair-wise comparisons are performed between each possible pair
		 * of the four coordinates, and the results are used to rank the numbers.
		 */
		var rx = 0
		var ry = 0
		var rz = 0
		var rw = 0
		if (x0 > y0) rx += 1 else ry += 1
		if (x0 > z0) rx += 1 else rz += 1
		if (x0 > w0) rx += 1 else rw += 1
		if (y0 > z0) ry += 1 else rz += 1
		if (y0 > w0) ry += 1 else rw += 1
		if (z0 > w0) rz += 1 else rw += 1

		/*
		 * simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
		 * Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
		 * impossible. Only the 24 indices which have non-zero entries make any sense.
		 * We use a threshold to set the coordinates in turn from the largest magnitude.
		 */

		// Rank 3 denotes the largest coordinate.
		val i1 = if (rx >= 3) 1 else 0
		val j1 = if (ry >= 3) 1 else 0
		val k1 = if (rz >= 3) 1 else 0
		val l1 = if (rw >= 3) 1 else 0
		// Rank 2 denotes the second largest coordinate.
		val i2 = if (rx >= 2) 1 else 0
		val j2 = if (ry >= 2) 1 else 0
		val k2 = if (rz >= 2) 1 else 0
		val l2 = if (rw >= 2) 1 else 0
		// Rank 1 denotes the second smallest coordinate.
		val i3 = if (rx >= 1) 1 else 0
		val j3 = if (ry >= 1) 1 else 0
		val k3 = if (rz >= 1) 1 else 0
		val l3 = if (rw >= 1) 1 else 0



		// The fifth corner has all coordinate offsets = 1, so no need to compute that.
		// Offsets for second corner in (x,y,z,w) coords
		val x1 = x0 - i1 + G4
		val y1 = y0 - j1 + G4
		val z1 = z0 - k1 + G4
		val w1 = w0 - l1 + G4
		// Offsets for third corner in (x,y,z,w) coords
		val x2 = x0 - i2 + 2.0 * G4
		val y2 = y0 - j2 + 2.0 * G4
		val z2 = z0 - k2 + 2.0 * G4
		val w2 = w0 - l2 + 2.0 * G4
		// Offsets for fourth corner in (x,y,z,w) coords
		val x3 = x0 - i3 + 3.0 * G4
		val y3 = y0 - j3 + 3.0 * G4
		val z3 = z0 - k3 + 3.0 * G4
		val w3 = w0 - l3 + 3.0 * G4
		// Offsets for last corner in (x,y,z,w) coords
		val x4 = x0 - 1.0 + 4.0 * G4
		val y4 = y0 - 1.0 + 4.0 * G4
		val z4 = z0 - 1.0 + 4.0 * G4
		val w4 = w0 - 1.0 + 4.0 * G4

		// Work out the hashed gradient indices of the five simplex corners
		val ii = i & 255
		val jj = j & 255
		val kk = k & 255
		val ll = l & 255

		var (n0, n1) = (0.0, 0.0)
		var (n2, n3) = (0.0, 0.0)
		var n4 = 0.0

		var t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0
		if (t0 >= 0) {
			val gi0 = perm(ii + perm(jj + perm(kk + perm(ll)))) % 32
			t0 *= t0
			n0 = t0 * t0 * dot(grad4(gi0), x0, y0, z0, w0)
		}
		var t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1
		if (t1 >= 0) {
			val gi1 = perm(ii + i1 + perm(jj + j1 + perm(kk + k1 + perm(ll + l1)))) % 32
			t1 *= t1
			n1 = t1 * t1 * dot(grad4(gi1), x1, y1, z1, w1)
		}
		var t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2
		if (t2 >= 0) {
			val gi2 = perm(ii + i2 + perm(jj + j2 + perm(kk + k2 + perm(ll + l2)))) % 32
			t2 *= t2
			n2 = t2 * t2 * dot(grad4(gi2), x2, y2, z2, w2)
		}
		var t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3
		if (t3 >= 0) {
			val gi3 = perm(ii + i3 + perm(jj + j3 + perm(kk + k3 + perm(ll + l3)))) % 32
			t3 *= t3
			n3 = t3 * t3 * dot(grad4(gi3), x3, y3, z3, w3)
		}
		var t4 = 0.6 - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4
		if (t4 >= 0) {
			val gi4 = perm(ii + 1 + perm(jj + 1 + perm(kk + 1 + perm(ll + 1)))) % 32
			t4 *= t4
			n4 = t4 * t4 * dot(grad4(gi4), x4, y4, z4, w4)
		}

		// Sum up and scale the result to cover the range [-1,1]
		27.0 * (n0 + n1 + n2 + n3 + n4)
	}
}

object Simplex {
	private final val F2 = 0.5 * (sqrt(3.0) - 1.0)
	private final val G2 = (3.0 - sqrt(3.0)) / 6.0
	private final val F3 = 1.0 / 3.0
	private final val G3 = 1.0 / 6.0
	private final val F4 = (math.sqrt(5.0) - 1.0) / 4.0
	private final val G4 = (5.0 - math.sqrt(5.0)) / 20.0

	private def dot(g: Grad, x: Double, y: Double) = g.x * x + g.y * y

	private def dot(g: Grad, x: Double, y: Double, z: Double) = g.x * x + g.y * y + g.z * z

	private def dot(g: Grad, x: Double, y: Double, z: Double, w: Double) = g.x * x + g.y * y + g.z * z + g.w * w

	private def floor(x: Double) = x.toInt

	private final val grad3 = Array(
		Grad(1, 1, 0), Grad(-1, 1, 0), Grad(1, -1, 0), Grad(-1, -1, 0),
		Grad(1, 0, 1), Grad(-1, 0, 1), Grad(1, 0, -1), Grad(-1, 0, -1),
		Grad(0, 1, 1), Grad(0, -1, 1), Grad(0, 1, -1), Grad(0, -1, -1)
	)
	private final val grad4 = Array(
		Grad(0, 1, 1, 1), Grad(0, 1, 1, -1), Grad(0, 1, -1, 1), Grad(0, 1, -1, -1),
		Grad(0, -1, 1, 1), Grad(0, -1, 1, -1), Grad(0, -1, -1, 1), Grad(0, -1, -1, -1),
		Grad(1, 0, 1, 1), Grad(1, 0, 1, -1), Grad(1, 0, -1, 1), Grad(1, 0, -1, -1),
		Grad(-1, 0, 1, 1), Grad(-1, 0, 1, -1), Grad(-1, 0, -1, 1), Grad(-1, 0, -1, -1),
		Grad(1, 1, 0, 1), Grad(1, 1, 0, -1), Grad(1, -1, 0, 1), Grad(1, -1, 0, -1),
		Grad(-1, 1, 0, 1), Grad(-1, 1, 0, -1), Grad(-1, -1, 0, 1), Grad(-1, -1, 0, -1),
		Grad(1, 1, 1, 0), Grad(1, 1, -1, 0), Grad(1, -1, 1, 0), Grad(1, -1, -1, 0),
		Grad(-1, 1, 1, 0), Grad(-1, 1, -1, 0), Grad(-1, -1, 1, 0), Grad(-1, -1, -1, 0)
	)

	private case class Grad(x: Byte, y: Byte, z: Byte, w: Byte = 0) {
		override def toString = s"($x,$y,$z,$w)"
	}

}