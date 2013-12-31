package factorygame.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import factorygame.TestBase
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner])
class ColorTest extends TestBase {
	final val ColorGen = for {
		r <- Gen.choose(0, 255)
		g <- Gen.choose(0, 255)
		b <- Gen.choose(0, 255)
		a <- Gen.choose(0, 255)
	} yield (Color(r, g, b, a), (r, g, b, a))

	"Color creation" must "be reversible" in {
		forAll(ColorGen)(v => {
			val col = v._1
			val num = v._2
			println(col, num)
			(col.r, col.g, col.b, col.a) === num
		})
	}
}
