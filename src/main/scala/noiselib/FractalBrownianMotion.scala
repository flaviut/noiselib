package noiselib

import spire.syntax.cfor._

/**
 * Based on https://code.google.com/p/fractalterraingeneration/wiki/Fractional_Brownian_Motion
 * Generates detailed fractal noise out of any noise source
 */
trait FractalBrownianMotion extends NoiseSource {
	val octaves   : Int    = 6
	val frequency : Double = 1.0 / 32.0
	val gain      : Double = 0.5
	val lacunarity: Double = 3.0

	abstract override def gen(x: Double, y: Double) = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += super.gen(x * freq, y * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out
	}

	abstract override def gen(x: Double, y: Double, z: Double) = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += super.gen(x * freq, y * freq, z * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out
	}

	abstract override def gen(x: Double, y: Double, z: Double, w: Double) = {
		var out = 0.0
		var freq = frequency
		var amplitude = gain

		cfor(0)(_ < octaves, _ + 1)(i => {
			out += super.gen(x * freq, y * freq, z * freq, w * freq) * amplitude
			freq *= lacunarity
			amplitude *= gain
		})

		out
	}
}