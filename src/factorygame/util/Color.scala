package factorygame.util


class Color(val rgba: Int) extends AnyVal {
	def fr = r.toFloat / 255
	def fg = g.toFloat / 255
	def fb = b.toFloat / 255
	def fa = a.toFloat / 255

	def r = rgba >> 24 & 0xFF
	def g = rgba >> 16 & 0xFF
	def b = rgba >> 8 & 0xFF
	def a = rgba & 0xFF

	def +(v: Long) = update((r + v).toInt, (g + v).toInt, (b + v).toInt)
	def -(v: Long) = update((r - v).toInt, (g - v).toInt, (b - v).toInt)

	def +(v: Float) = fupdate(fr + v, fg + v, fb + v)
	def -(v: Float) = fupdate(fr - v, fg - v, fb - v)
	def *(v: Float) = fupdate(fr * v, fg * v, fb * v)
	def /(v: Float) = fupdate(fr / v, fg / v, fb / v)

	def +(v: Color) = update(r + v.r, g + v.g, b + v.b)
	def -(v: Color) = update(r - v.r, g - v.g, b - v.b)
	def *(v: Color) = fupdate(fr * v.fr, fg * v.fg, fb * v.fb)
	def /(v: Color) = fupdate(fr / v.fr, fg / v.fg, fb / v.fb)

	def update(newR: Int = r, newG: Int = g, newB: Int = b, newA: Int = a): Color =
		Color(newR, newG, newB, newA)

	def fupdate(newR: Float = r, newG: Float = g, newB: Float = b, newA: Float = a): Color =
		Color(newR, newG, newB, newA)

	override def toString = s"Color(r=$r,g=$g,b=$b,a=$a)"
}

object Color {
	def apply(r: Int, g: Int, b: Int): Color =
		Color(r, g, b, 255)

	def apply(r: Int, g: Int, b: Int, a: Int): Color =
		new Color(r << 24 | g << 16 | b << 8 | a)

	def apply(r: Float, g: Float, b: Float): Color =
		Color(
			(r * 255).toInt & 0xFF,
			(g * 255).toInt & 0xFF,
			(b * 255).toInt & 0xFF)

	def apply(r: Float, g: Float, b: Float, a: Float): Color =
		Color(
			(r * 255).toInt & 0xFF,
			(g * 255).toInt & 0xFF,
			(b * 255).toInt & 0xFF,
			(a * 255).toInt & 0xFF)

	def apply(c: java.awt.Color): Color =
		Color(
			c.getRed,
			c.getGreen,
			c.getBlue,
			c.getAlpha)

	def apply(c: org.lwjgl.util.Color): Color =
		Color(
			c.getRed,
			c.getGreen,
			c.getBlue,
			c.getAlpha)
}