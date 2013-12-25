package factorygame.util

import Gens._
import factorygame.TestBase
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class BitPackingTests extends TestBase {

	"Byte sets" must "have inverse getters and setters" in {

		forAll(bytes8)(n => {
			val t = Bytes(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7))
			t.b1 === n(0)
			t.b2 === n(1)
			t.b3 === n(2)
			t.b4 === n(3)
			t.b5 === n(4)
			t.b6 === n(5)
			t.b7 === n(6)
			t.b8 === n(7)
		})

		forAll(bytes4)(n => {
			val t = Bytes(n(0), n(1), n(2), n(3))
			t.b1 === n(0)
			t.b2 === n(1)
			t.b3 === n(2)
			t.b4 === n(3)
		})

		forAll(bytes2)(n => {
			val t = Bytes(n(0), n(1))
			t.b1 === n(0)
			t.b2 === n(1)
		})
	}

	"Nibble sets" must "have inverse getters and setters" in {

		forAll(nibbles16)((b: Array[Byte]) => {
			val t = Nibbles(b(0), b(1), b(2), b(3), b(4), b(5), b(6), b(7),
				b(8), b(9), b(10), b(11), b(12), b(13), b(14), b(15))
			t.n1 === b(0)
			t.n2 === b(1)
			t.n3 === b(2)
			t.n4 === b(3)
			t.n5 === b(4)
			t.n6 === b(5)
			t.n7 === b(6)
			t.n8 === b(7)
			t.n9 === b(8)
			t.n10 === b(9)
			t.n11 === b(10)
			t.n12 === b(11)
			t.n13 === b(12)
			t.n14 === b(13)
			t.n15 === b(14)
			t.n16 === b(15)
		})
	}

	forAll(nibbles8)((b: Array[Byte]) => {
		val t = Nibbles(b(0), b(1), b(2), b(3), b(4), b(5), b(6), b(7))
		t.n1 === b(0)
		t.n2 === b(1)
		t.n3 === b(2)
		t.n4 === b(3)
		t.n5 === b(4)
		t.n6 === b(5)
		t.n7 === b(6)
		t.n8 === b(7)
	})

	forAll(nibbles4)((b: Array[Byte]) => {
		val t = Nibbles(b(0), b(1), b(2), b(3))
		t.n1 === b(0)
		t.n2 === b(1)
		t.n3 === b(2)
		t.n4 === b(3)
	})

	forAll(nibbles2)((b: Array[Byte]) => {
		val t = Nibbles(b(0), b(1))
		t.n1 === b(0)
		t.n2 === b(1)
	})

}
