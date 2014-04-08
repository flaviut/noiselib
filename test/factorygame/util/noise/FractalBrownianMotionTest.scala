package factorygame.util.noise

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import factorygame.{ParallelTestBase, TestBase}
import factorygame.util.noise.NoiseTest._

@RunWith(classOf[JUnitRunner]) class FractalBrownianMotionTest extends ParallelTestBase {
	val noise = new WhiteNoise with LinearInterp with FractalBrownianMotion

	"2D fractal brownian motion noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise2D(noise)
	}

	"3D fractal brownian motion noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise3D(noise)
	}

	"4D fractal brownian motion noise" must "only return values within [-1,1]" in {
		testOutOfBoundsNoise4D(noise)
	}
}
