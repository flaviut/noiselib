package factorygame.util.noise

import spire.syntax.cfor._
import scala.util.Random

/**
 * Based on https://code.google.com/p/fractalterraingeneration/wiki/Fractional_Brownian_Motion
 * Generates detailed fractal noise out of any noise source
 */
class FractalBrownianMotion(val noise: FBMOctave, octaves: Int, frequency: Double, gain: Double, lacunarity: Double) {

	def this(seed: Int) = this(new Simplex(seed), 6, 1.0 / 32.0, 0.5, 3.0)

	def this() = this(Random.nextInt())

	def gen(x: Double, y: Double) = {
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

	def gen(x: Double, y: Double, z: Double) = {
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

	def gen(x: Double, y: Double, z: Double, w: Double) = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += noise.gen(x * freq, y * freq, z * freq, w * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out
	}
}

/**
 * Used by implementers to signify they are compatible with FractalBrownianMotion
 */
trait FBMOctave {
	def gen(x: Double, y: Double): Double
	def gen(x: Double, y: Double, z: Double): Double
	def gen(x: Double, y: Double, z: Double, w: Double): Double
}
