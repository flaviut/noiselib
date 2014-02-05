package factorygame.util.noise

import factorygame.TestBase
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import factorygame.util.noise.NoiseTest._

@RunWith(classOf[JUnitRunner]) class SeamlessNoiseTest extends TestBase {
	val seamlessWhite = new WhiteNoise(Random.nextInt()) with LinearInterp with SeamlessNoise {
		override val width  = 512
		override val height = 512
		override val scale  = 1.0
	}

	val seamlessSimplex = new Simplex(Random.nextInt()) with SeamlessNoise {
		override val width  = 512
		override val height = 512
		override val scale  = 1.0
	}

	"Seamless white noise" must "actually be seamless" in {
		for (i <- 0 until 512) {
			seamlessWhite.gen(i, 0) mustEqual (seamlessWhite.gen(i, 513) +- 0.01)
			seamlessWhite.gen(0, i) mustEqual (seamlessWhite.gen(513, i) +- 0.01)
		}
	}

	"Seamless simplex noise" must "actually be seamless" in {
		for (i <- 0 until 512) {
			seamlessSimplex.gen(i, 0) mustEqual (seamlessSimplex.gen(i, 513) +- 0.01)
			seamlessSimplex.gen(0, i) mustEqual (seamlessSimplex.gen(513, i) +- 0.01)
		}
	}

	"Seamless white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(seamlessWhite)
	}

	"Seamless simplex noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(seamlessSimplex)
	}
}
