package factorygame.util

import Bytes._

/**
 * 0 aligned from the right, inclusive. Used to add a layer of abstraction on performing BitOps
 */
class BitRange(val v: Short) extends AnyVal {
	def start: Byte = v.b1

	def end: Byte = v.b2

	def size: Byte = (end - start).toByte

	def lMask: Long = {
		require(start >= 0 && end < 64, s"Invalid range $start, $end; should be start >= 0, end < 64")
		((1 << size + 1) - 1) << start
	}

	def iMask: Int = {
		require(start >= 0 && end < 32, s"Invalid range $start, $end; should be start >= 0, end < 32")
		lMask.toInt
	}

	def sMask: Short = {
		require(start >= 0 && end < 16, s"Invalid range $start, $end; should be start >= 0, end < 16")
		lMask.toShort
	}

	def bMask: Byte = {
		require(start >= 0 && end < 8, s"Invalid range $start, $end; should be start >= 0, end < 8")
		lMask.toByte
	}

	override def toString = s"BitRange($start, $end)"
}

object BitRange {
	def apply(start: Byte, end: Byte) = {
		require(start >= 0 && end < 64, s"Invalid bounds $start, $end; should be start >= 0, end < 64")
		require(end > start, s"End value of $end should not be lower than the start value of $start")
		new BitRange(Bytes(start, end).v)
	}
}

/**
 * Implicit wrappers to add a layer of abstraction to packing data as bytes in larger-size data types
 */
object Bytes {
	def apply(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte) =
		new LongBytes(b8 << 56 | b7 << 48 | b6 << 40 | b5 << 32 | b4 << 24 | b3 << 16 | b2 << 8 | b1)

	def apply(b1: Byte, b2: Byte, b3: Byte, b4: Byte) =
		new IntBytes(b4 << 24 | b3 << 16 | b2 << 8 | b1)

	def apply(b1: Byte, b2: Byte) =
		new ShortBytes((b2 << 8 | b1).toShort)

	implicit class LongBytes(val v: Long) extends AnyVal {
		def b1 = apply(1)

		def b2 = apply(2)

		def b3 = apply(3)

		def b4 = apply(4)

		def b5 = apply(5)

		def b6 = apply(6)

		def b7 = apply(7)

		def b8 = apply(8)

		def b1_=(newV: Byte) = update(b1 = newV)

		def b2_=(newV: Byte) = update(b2 = newV)

		def b3_=(newV: Byte) = update(b3 = newV)

		def b4_=(newV: Byte) = update(b4 = newV)

		def b5_=(newV: Byte) = update(b5 = newV)

		def b6_=(newV: Byte) = update(b6 = newV)

		def b7_=(newV: Byte) = update(b7 = newV)

		def b8_=(newV: Byte) = update(b8 = newV)

		def apply(n: Int): Byte = {
			if (n < 1 || n > 8) {
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,8]")
			}
			((v >>> 8 * (n - 1)) & 0xFF).toByte
		}

		def update(b1: Byte = b1, b2: Byte = b2, b3: Byte = b2, b4: Byte = b2,
		           b5: Byte = b5, b6: Byte = b6, b7: Byte = b7, b8: Byte = b8): LongBytes = {
			Bytes(b1, b2, b3, b4, b5, b6, b7, b8)
		}
	}

	implicit class IntBytes(val v: Int) extends AnyVal {
		def b1 = {
			apply(1)
		}

		def b2 = {
			apply(2)
		}

		def b3 = {
			apply(3)
		}

		def b4 = {
			apply(4)
		}

		def b1_=(newV: Byte) = {
			update(b1 = newV)
		}

		def b2_=(newV: Byte) = {
			update(b2 = newV)
		}

		def b3_=(newV: Byte) = {
			update(b3 = newV)
		}

		def b4_=(newV: Byte) = {
			update(b4 = newV)
		}

		def apply(n: Int): Byte = {
			if (n < 1 || n > 4) {
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,4]")
			}
			((v >>> 8 * n) & 0xFF).toByte
		}

		def update(b1: Byte = b1, b2: Byte = b2, b3: Byte = b2, b4: Byte = b2): IntBytes = {
			Bytes(b1, b2, b3, b4)
		}
	}

	implicit class ShortBytes(val v: Short) extends AnyVal {
		def b1 = {
			apply(1)
		}

		def b2 = {
			apply(2)
		}

		def b1_=(newV: Byte) = {
			update(b1 = newV)
		}

		def b2_=(newV: Byte) = {
			update(b2 = newV)
		}

		def apply(n: Int): Byte = {
			if (n < 1 || n > 2) {
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,2]")
			}
			((v >>> 8 * n) & 0xFF).toByte
		}

		def update(b1: Byte = b1, b2: Byte = b2) = {
			Bytes(b1, b2)
		}
	}

}

/**
 * Implicit wrappers to add a layer of abstraction for packing data as nibbles in larger-size data types
 */
object Nibbles {

	def apply(n1: Byte, n2: Byte, n3: Byte, n4: Byte, n5: Byte, n6: Byte, n7: Byte, n8: Byte,
	          n9: Byte, n10: Byte, n11: Byte, n12: Byte, n13: Byte, n14: Byte, n15: Byte, n16: Byte) =
		new LongNibbles(
			(n16 & 0xF) << 60 | (n15 & 0xF) << 56 | (n14 & 0xF) << 52 | (n13 & 0xF) << 48 |
				(n12 & 0xF) << 44 | (n11 & 0xF) << 40 | (n10 & 0xF) << 36 | (n9 & 0xF) << 32 |
				(n8 & 0xF) << 28 | (n7 & 0xF) << 24 | (n6 & 0xF) << 20 | (n5 & 0xF) << 16 |
				(n4 & 0xF) << 12 | (n3 & 0xF) << 8 | (n2 & 0xF) << 4 | (n1 & 0xF))


	def apply(n1: Byte, n2: Byte, n3: Byte, n4: Byte, n5: Byte, n6: Byte, n7: Byte, n8: Byte) =
		new IntNibbles(
			(n8 & 0xF) << 28 | (n7 & 0xF) << 24 | (n6 & 0xF) << 20 | (n5 & 0xF) << 16 |
				(n4 & 0xF) << 12 | (n3 & 0xF) << 8 | (n2 & 0xF) << 4 | (n1 & 0xF))

	def apply(n1: Byte, n2: Byte, n3: Byte, n4: Byte) =
		new ShortNibbles(((n4 & 0xF) << 12 | (n3 & 0xF) << 8 | (n2 & 0xF) << 4 | (n1 & 0xF)).toShort)

	def apply(n1: Byte, n2: Byte) =
		new ByteNibbles(((n2 & 0xF) << 4 | (n1 & 0xF)).toByte)

	implicit class LongNibbles(val v: Long) extends AnyVal {
		def n1 = apply(1)

		def n2 = apply(2)

		def n3 = apply(3)

		def n4 = apply(4)

		def n5 = apply(5)

		def n6 = apply(6)

		def n7 = apply(7)

		def n8 = apply(8)

		def n9 = apply(9)

		def n10 = apply(10)

		def n11 = apply(11)

		def n12 = apply(12)

		def n13 = apply(13)

		def n14 = apply(14)

		def n15 = apply(15)

		def n16 = apply(16)

		def n1_=(newV: Byte) = update(n1 = newV)

		def n2_=(newV: Byte) = update(n2 = newV)

		def n3_=(newV: Byte) = update(n3 = newV)

		def n4_=(newV: Byte) = update(n4 = newV)

		def n5_=(newV: Byte) = update(n5 = newV)

		def n6_=(newV: Byte) = update(n6 = newV)

		def n7_=(newV: Byte) = update(n7 = newV)

		def n8_=(newV: Byte) = update(n8 = newV)

		def n9_=(newV: Byte) = update(n9 = newV)

		def n10_=(newV: Byte) = update(n10 = newV)

		def n11_=(newV: Byte) = update(n11 = newV)

		def n12_=(newV: Byte) = update(n12 = newV)

		def n13_=(newV: Byte) = update(n13 = newV)

		def n14_=(newV: Byte) = update(n14 = newV)

		def n15_=(newV: Byte) = update(n15 = newV)

		def n16_=(newV: Byte) = update(n16 = newV)

		def apply(n: Int): Byte = {
			if (n < 1 || n > 16)
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,16]")
			((v >>> 4 * (n - 1)) & 0xF).toByte
		}

		def update(n1: Byte = n1, n2: Byte = n2, n3: Byte = n3, n4: Byte = n4,
		           n5: Byte = n5, n6: Byte = n6, n7: Byte = n7, n8: Byte = n8,
		           n9: Byte = n9, n10: Byte = n10, n11: Byte = n11, n12: Byte = n12,
		           n13: Byte = n13, n14: Byte = n14, n15: Byte = n15, n16: Byte = n16) = {
			Nibbles(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
		}
	}

	implicit class IntNibbles(val v: Int) extends AnyVal {
		def n1 = apply(1)

		def n2 = apply(2)

		def n3 = apply(3)

		def n4 = apply(4)

		def n5 = apply(5)

		def n6 = apply(6)

		def n7 = apply(7)

		def n8 = apply(8)

		def n1_=(newV: Byte) = update(n1 = newV)

		def n2_=(newV: Byte) = update(n2 = newV)

		def n3_=(newV: Byte) = update(n3 = newV)

		def n4_=(newV: Byte) = update(n4 = newV)

		def n5_=(newV: Byte) = update(n5 = newV)

		def n6_=(newV: Byte) = update(n6 = newV)

		def n7_=(newV: Byte) = update(n7 = newV)

		def n8_=(newV: Byte) = update(n8 = newV)

		def apply(n: Int): Byte = {
			if (n < 1 || n > 8)
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,8]")
			((v >>> 4 * (n - 1)) & 0xF).toByte
		}

		def update(n1: Byte = n1, n2: Byte = n2, n3: Byte = n3, n4: Byte = n4,
		           n5: Byte = n5, n6: Byte = n6, n7: Byte = n7, n8: Byte = n8) = {
			Nibbles(n1, n2, n3, n4, n5, n6, n7, n8)
		}
	}

	implicit class ShortNibbles(val v: Short) extends AnyVal {
		def n1 = apply(1)

		def n2 = apply(2)

		def n3 = apply(3)

		def n4 = apply(4)

		def n1_=(newV: Byte) = update(n1 = newV)

		def n2_=(newV: Byte) = update(n2 = newV)

		def n3_=(newV: Byte) = update(n3 = newV)

		def n4_=(newV: Byte) = update(n4 = newV)

		def apply(n: Int): Byte = {
			if (n < 1 || n > 8)
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,4]")
			((v >>> 4 * (n - 1)) & 0xF).toByte
		}

		def update(n1: Byte = n1, n2: Byte = n2, n3: Byte = n3, n4: Byte = n4) = {
			Nibbles(n1, n2, n3, n4)
		}
	}

	implicit class ByteNibbles(val v: Byte) extends AnyVal {
		def n1 = apply(1)

		def n2 = apply(2)

		def n1_=(newV: Byte) = update(n1 = newV)

		def n2_=(newV: Byte) = update(n2 = newV)

		def apply(n: Int): Byte = {
			if (n < 1 || n > 8)
				throw new IndexOutOfBoundsException(s"Index out of range: $n, should be in [1,2]")
			((v >>> 4 * (n - 1)) & 0xF).toByte
		}

		def update(n1: Byte = n1, n2: Byte = n2) = {
			Nibbles(n1, n2)
		}
	}

}
