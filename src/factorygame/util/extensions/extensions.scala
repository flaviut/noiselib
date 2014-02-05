package factorygame.util

import com.badlogic.gdx.graphics.Pixmap

package object extensions {

	implicit class EnhancedPixmap(pix: Pixmap) extends Buffer2D[Color] {
		def apply(x: Int, y: Int) = new Color(pix.getPixel(x, y))

		def update(x: Int, y: Int, col: Color) = pix.drawPixel(x, y, col.rgba)

		def width = pix.getWidth

		def height = pix.getHeight
	}

}
