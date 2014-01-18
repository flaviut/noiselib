package factorygame.util

/**
 * PRNG that returns the same thing for any call with the same seed and position
 * Seems to be uniformly distributed
 */
class PositionalRandom(seed: Int = 0) {

	import scala.util.hashing.MurmurHash3._

	def get(x: Int, y: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mixLast(h, y)
		finalizeHash(h, 2)
	}

	def get(x: Int, y: Int, z: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mix(h, y)
		h = mixLast(h, z)
		finalizeHash(h, 3)
	}

	def get(x: Int, y: Int, z: Int, w: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mix(h, y)
		h = mix(h, z)
		h = mixLast(h, w)
		finalizeHash(h, 4)
	}
}
