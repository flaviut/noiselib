package factorygame.util.noise

abstract class NoiseSource {
	def gen(x: Double, y: Double): Double

	def gen(x: Double, y: Double, z: Double): Double

	def gen(x: Double, y: Double, z: Double, w: Double): Double
}
