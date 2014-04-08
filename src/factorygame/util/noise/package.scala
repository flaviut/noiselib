package factorygame.util

package object noise {
	private[noise] def floor(x: Double) = if (x > 0) x.toInt else x.toInt - 1
}
