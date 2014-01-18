package factorygame.util.noise

import factorygame.util.Array2D

class DiamondSquare(seed: Int) {
	def gen(size: Int, seeds: Option[(Int, Int) Map Double] = None): Double = {
		val arr = new Array2D[Double](size)
		val rand = new WhiteNoise(seed)
		seeds.foreach(_.foreach(v =>
			arr(v._1) = v._2
		))

		???
	}
}
