package noiselib

import org.scalacheck.Gen
import org.scalatest.MustMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import scala.util.Random
import noiselib.util.geom._

object NoiseTest extends MustMatchers {
	implicit val Point2dGen =
		for {
			x <- Gen.chooseNum(-500000.0, 500000.0)
			y <- Gen.chooseNum(-500000.0, 500000.0)
		} yield Vec2(x, y)

	implicit val Point3dGen =
		for {
			x <- Gen.chooseNum(-500000.0, 500000.0)
			y <- Gen.chooseNum(-500000.0, 500000.0)
			z <- Gen.chooseNum(-500000.0, 500000.0)
		} yield Vec3(x, y, z)

	implicit val Point4dGen =
		for {
			x <- Gen.chooseNum(-500000.0, 500000.0)
			y <- Gen.chooseNum(-500000.0, 500000.0)
			z <- Gen.chooseNum(-500000.0, 500000.0)
			w <- Gen.chooseNum(-500000.0, 500000.0)
		} yield Vec4(x, y, z, w)

	val NoiseTypes = List(
		new WhiteNoise(Random.nextInt()) with LinearInterp,
		new WhiteNoise(Random.nextInt()) with NearestNeighborInterp,
		new WhiteNoise(Random.nextInt()) with LinearInterp with FractalBrownianMotion,
		new Simplex(Random.nextInt())
	)

	def testOutOfBoundsNoise2D(n: NoiseSource) =
		forAll(Point2dGen)(p => {
			n.gen(p.x, p.y) mustEqual (0.0 +- 1.0)
		})

	def testOutOfBoundsNoise3D(n: NoiseSource) =
		forAll(Point3dGen)(p => {
			n.gen(p.x, p.y, p.z) mustEqual (0.0 +- 1.0)
		})

	def testOutOfBoundsNoise4D(n: NoiseSource) =
		forAll(Point4dGen)(p => {
			n.gen(p.x, p.y, p.z, p.w) mustEqual (0.0 +- 1.0)
		})


}
