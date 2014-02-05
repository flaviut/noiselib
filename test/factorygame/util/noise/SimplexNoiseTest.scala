package factorygame.util.noise

import factorygame.util.noise.NoiseTest._
import scala.util.Random
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import factorygame.TestBase

@RunWith(classOf[JUnitRunner]) class SimplexNoiseTest extends TestBase {
	val simplexNoise = new Simplex(Random.nextInt())

	"2D simplex noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(simplexNoise)
	}

	"3D simplex noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise3D(simplexNoise)
	}

	"4D simplex noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise4D(simplexNoise)
	}
}
