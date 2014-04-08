package noiselib.util

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Extensions {

	implicit class EnhancedImage(pix: BufferedImage) extends Buffer2D[Color] {
		override def update(x: Int, y: Int, v: Color) = pix.setRGB(x, y, v.rgba)
		override def apply(x: Int, y: Int) = new Color(pix.getRGB(x, y))
		override def height = pix.getHeight
		override def width = pix.getWidth

		def save(file: File, format: String) = ImageIO.write(pix, format, file)
		def save(file: String, format: String) = ImageIO.write(pix, format, new File(file))
	}

}
