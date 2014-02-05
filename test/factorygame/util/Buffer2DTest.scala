package factorygame
package util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import factorygame.TestBase
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner]) class Buffer2DTest extends TestBase {
	implicit val Array2DGen = for {
		w <- Gen.choose(1, 50)
		h <- Gen.choose(1, 50)
	} yield new Array2D[Double](w, h)

	"Iterating through each element in parallel" must "access width*height elements" in {
		forAll(Array2DGen)(arr => {
			arr.par.aggregate(0)((b, _) => b + 1, _ + _) mustEqual arr.size
		})
	}

	"Parallel iteration" must "be faster than sequential processing" in {
		val arr = new Array2D[Int](500, 500)
		val parArr = arr.par
		val rand = new PositionalRandom()

		val tTime = compareFuncs(
			() => parArr.tabulate(v => rand.get(v.x, v.y)),
			() => arr.tabulate(v => rand.get(v.x, v.y)))
		val mTime = compareFuncs(
			() => parArr.reduce((p, n) => Indexer2D(0, 0, FastMath.atan2(p.v, n.v).toInt)),
			() => arr.reduce((p, n) => Indexer2D(0, 0, FastMath.atan2(p.v, n.v).toInt)))

		println(tTime)
		tTime._1 mustBe <(tTime._2)
		println(mTime)
		mTime._1 mustBe <(mTime._2)
	}
}
