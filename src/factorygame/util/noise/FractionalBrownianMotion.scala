package factorygame.util.noise

import spire.syntax.cfor._
import scala.util.Random

/* Based on http://goo.gl/UdUu7C */
class FractionalBrownianMotion(noise: Noise,
                               octaves: Int,
                               frequency: Double,
                               gain: Double,
                               lacunarity: Double) extends Noise {
	def this(noise: Noise) = this(noise, 6, 1.0 / 16.0, 0.5, 2.0)

	def this() = this(new Simplex(Random.nextInt()))

	def gen(x: Double, y: Double): Double = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += noise.gen(x * freq, y * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out
	}

	def gen(x: Double, y: Double, z: Double): Double = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += noise.gen(x * freq, y * freq, z * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out

	}
}
