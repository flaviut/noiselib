package factorygame.util.noise

trait Noise {
	def gen(x: Double, y: Double): Double
	def gen(x: Double, y: Double, z: Double): Double
}