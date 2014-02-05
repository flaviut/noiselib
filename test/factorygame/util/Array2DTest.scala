package factorygame.util

import factorygame.TestBase
import org.scalacheck.Gen
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class Array2DTest extends TestBase {
	implicit val Array2DGen = for {
		w <- Gen.choose(1, 50)
		h <- Gen.choose(1, 50)
	} yield new Array2D[Double](w, h)

	"A 2d array" must "have a size of width*height" in {
		forAll(Array2DGen)(arr => {
			arr.size mustEqual arr.width * arr.height
		})
	}

	"A 2d array of size width, height" must "have width*height elements" in {
		forAll(Array2DGen)(arr => {
			arr.foldLeft(0)((i, _) => i + 1) mustEqual arr.size
		})
	}

	"Placing something in a 2d array" must "persist across accesses" in {
		forAll(Array2DGen)(arr => {
			val locs = for {Indexer2D(x, y, _) <- arr
			                v = Random.nextDouble()} yield {arr(x, y) = v; Indexer2D(x, y, v)}
			for (pos <- locs)
				pos.v mustEqual arr(pos.x, pos.y)
		})
	}

	"Accessing out of bounds elements" must "throw an exception" in {
		forAll(Array2DGen)(arr => {
			val width = arr.width
			val height = arr.height
			intercept[IndexOutOfBoundsException] {arr(width, 0)}
			intercept[IndexOutOfBoundsException] {arr(0, height)}
			intercept[IndexOutOfBoundsException] {arr(-1, 0)}
			intercept[IndexOutOfBoundsException] {arr(0, -1)}
		})
	}
}
