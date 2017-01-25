package noiselib.util

class Color(val rgba: Int) extends AnyVal {
	def fr: Float = r.toFloat / 255
	def fg: Float = g.toFloat / 255
	def fb: Float = b.toFloat / 255
	def fa: Float = a.toFloat / 255

	def r: Int = rgba >> 24 & 0xFF
	def g: Int = rgba >> 16 & 0xFF
	def b: Int = rgba >> 8 & 0xFF
	def a: Int = rgba & 0xFF

	def +(v: Long): Color = update((r + v).toInt, (g + v).toInt, (b + v).toInt)
	def -(v: Long): Color = update((r - v).toInt, (g - v).toInt, (b - v).toInt)

	def +(v: Double): Color = fupdate(fr + v, fg + v, fb + v)
	def -(v: Double): Color = fupdate(fr - v, fg - v, fb - v)
	def *(v: Double): Color = fupdate(fr * v, fg * v, fb * v)
	def /(v: Double): Color = fupdate(fr / v, fg / v, fb / v)

	def +(v: Color): Color = update(r + v.r, g + v.g, b + v.b)
	def -(v: Color): Color = update(r - v.r, g - v.g, b - v.b)
	def *(v: Color): Color = fupdate(fr * v.fr, fg * v.fg, fb * v.fb)
	def /(v: Color): Color = fupdate(fr / v.fr, fg / v.fg, fb / v.fb)

	def update(newR: Int = r, newG: Int = g, newB: Int = b, newA: Int = a): Color =
		Color(newR, newG, newB, newA)

	def fupdate(newR: Double = r, newG: Double = g, newB: Double = b, newA: Double = a): Color =
		Color(newR, newG, newB, newA)

	override def toString = s"Color(r=$r,g=$g,b=$b,a=$a)"
}

object Color {
	def apply(r: Int, g: Int, b: Int): Color =
		Color(r, g, b, 255)

	def apply(r: Int, g: Int, b: Int, a: Int): Color =
		new Color(r << 24 | g << 16 | b << 8 | a)

	def apply(r: Double, g: Double, b: Double): Color =
		Color(
			(r * 255).toInt & 0xFF,
			(g * 255).toInt & 0xFF,
			(b * 255).toInt & 0xFF)

	def apply(r: Double, g: Double, b: Double, a: Double): Color =
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
}