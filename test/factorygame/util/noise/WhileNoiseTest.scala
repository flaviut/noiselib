package factorygame.util.noise

import factorygame.TestBase
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import NoiseTest._

@RunWith(classOf[JUnitRunner]) class WhileNoiseTest extends TestBase {
	val linInterNoise = new WhiteNoise(Random.nextInt()) with LinearInterp
	val neaInterNoise = new WhiteNoise(Random.nextInt()) with NearestNeighborInterp

	"2D linearly interpolated white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(linInterNoise)
	}

	"3D linearly interpolated white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise3D(linInterNoise)
	}

	"4D linearly interpolated white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise4D(linInterNoise)
	}

	"2D nearest neighbor interpolated white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(neaInterNoise)
	}

	"3D nearest neighbor white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise3D(neaInterNoise)
	}

	"4D nearest neighbor white noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise4D(neaInterNoise)
	}
}
