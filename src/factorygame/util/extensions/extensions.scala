package factorygame.util

import com.badlogic.gdx.graphics.Pixmap
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

package object extensions {

	implicit class EnhancedPixmap(pix: Pixmap) extends Buffer2D[Color] {
		def apply(x: Int, y: Int) = new Color(pix.getPixel(x, y))
		def update(x: Int, y: Int, col: Color) = pix.drawPixel(x, y, col.rgba)
		def width = pix.getWidth
		def height = pix.getHeight
	}

	implicit class EnhancedImage(pix: BufferedImage) extends Buffer2D[Color] {
		override def update(x: Int, y: Int, v: Color) = pix.setRGB(x, y, v.rgba)
		override def apply(x: Int, y: Int) = new Color(pix.getRGB(x, y))
		override def height = pix.getHeight
		override def width = pix.getWidth

		def save(file: File, format: String) = ImageIO.write(pix, format, file)
		def save(file: String, format: String) = ImageIO.write(pix, format, new File(file))
	}

}
