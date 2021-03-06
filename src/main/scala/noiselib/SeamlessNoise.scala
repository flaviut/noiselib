package noiselib

import scala.math._

/**
 * Seamless noise in 2D. Is a bit slower because it samples
 * from 4D noise in the same way you can sample 1D seamless
 * noise from tracing a circle in 2D space
 *
 * Thanks to JTippetts, http://www.gamedev.net/blog/33/entry-2138456-seamless-noise/
 */
trait SeamlessNoise extends NoiseSource {
	val width : Int    = 512
	val height: Int    = 512
	val scale : Double = 20

	abstract override def gen(x: Double, y: Double): Double = {
		val u = x / width
		val v = y / height

		val nx = scale * cos(u * 2 * Pi) / (2 * Pi)
		val ny = scale * cos(v * 2 * Pi) / (2 * Pi)
		val nz = scale * sin(u * 2 * Pi) / (2 * Pi)
		val nw = scale * sin(v * 2 * Pi) / (2 * Pi)

		super.gen(nx, ny, nz, nw)
	}

	abstract override def gen(x: Double, y: Double, z: Double): Double = throw new UnsupportedOperationException

	abstract override def gen(x: Double, y: Double, z: Double, w: Double): Double = throw new UnsupportedOperationException
}
