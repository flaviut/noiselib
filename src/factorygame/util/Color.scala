package factorygame.util

import Bytes._

class Color(val rgba: Int) extends AnyVal {
	def r = rgba.b1

	def g = rgba.b2

	def b = rgba.b3

	def a = rgba.b4

	def r_=(newV: Int) = update(newR = newV & 0xFF)

	def g_=(newV: Int) = update(newG = newV & 0xFF)

	def b_=(newV: Int) = update(newB = newV & 0xFF)

	def a_=(newV: Int) = update(newA = newV & 0xFF)

	def +(v: Int) = {
		update(r + v, g + v, b + v)
	}

	def -(v: Int) = {
		update(r - v, g - v, b - v)
	}

	def update(newR: Int = r, newG: Int = g, newB: Int = b, newA: Int = a): Color = {
		Color(newR, newG, newB, newA)
	}

	override def toString = s"Color(r=$r,g=$g,b=$b,a=$a)"
}

object Color {
	def apply(r: Int, g: Int, b: Int, a: Int = 0): Color = {
		new Color(Bytes(r.toByte, g.toByte, b.toByte, a.toByte).v)
	}

	def apply(c: java.awt.Color): Color = {
		Color(c.getRed, c.getGreen, c.getBlue, c.getAlpha)
	}

	def apply(c: org.lwjgl.util.Color): Color = {
		Color(c.getRed, c.getGreen, c.getBlue, c.getAlpha)
	}
}