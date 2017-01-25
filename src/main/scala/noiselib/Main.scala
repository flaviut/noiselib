package noiselib

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Algorithms extends Enumeration {
  val Simplex, WhiteNoiseLinear, WhiteNoiseNearest = Value
}

case class Config(height: Int = 600, width: Int = 800,
                  output: File = new File("out.png"),
                  scale: Double = 10,
                  algorithm: Algorithms.Value = Algorithms.WhiteNoiseLinear,
                  seed: Int = 0,
                  shouldFbm: Boolean = false, shouldSeamless: Boolean = false)


object Main extends App {

  def saveImage(data: NoiseSource,
                width: Int, height: Int,
                scale: Double,
                file: File): Unit = {
    /** Save the data in data to a PNG image */
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)

    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val value = ((data.gen(x / scale, y / scale) + 1) * 255).toInt / 2
        val rgbValue: Int = value << 16 | value << 8 | value
        image.setRGB(x, y, rgbValue)
      }
    }

    ImageIO.write(image, "png", file)
  }

  val algorithmList = Algorithms.values.map(v => v.toString).mkString(", ")

  val parser = new scopt.OptionParser[Config]("noiselib") {
    opt[Int]('h', "height")
      .optional()
      .action((x, c) => c.copy(height = x))
      .text("image height")
    opt[Int]('w', "width")
      .optional()
      .action((x, c) => c.copy(width = x))
      .text("image width")
    opt[Double]("scale")
      .optional()
      .action((x, c) => c.copy(scale = x))
      .text("scale factor, the amount to zoom into the result")

    opt[File]('o', "out")
      .valueName("<file>")
      .optional()
      .action((x, c) => c.copy(output = x))
      .text("path to output image. Image must be a png.")

    opt[String]('a', "algorithm")
      .validate(arg => Algorithms.values.find(_.toString == arg) match {
        case Some(_) => Right()
        case None => Left(s"'$arg' is not a valid algorithm, expected one of $algorithmList")
      })
      .action((x, c) => c.copy(algorithm = Algorithms.withName(x)))
      .text(s"algorithm to use. Valid values are $algorithmList")

    opt[Int]('s', "seed")
      .optional()
      .action((x, c) => c.copy(seed = x))
      .text("algorithm seed")

    opt[Unit]('f', "fbm")
      .action((_, c) => c.copy(shouldFbm = true))
      .text("should fractal brownian motion be done?")
    // opt[Unit]('u', "seamless")
    //   .action((_, c) => c.copy(shouldSeamless = true))
    //   .text("should the result be made seamless? Note: this requires an algorithm that supports 4D noise")
  }

  parser.parse(args, Config()) match {
    case Some(config) =>
      val algo = config.algorithm match {
        // TODO: Inheritance is a bad way to do this. It should be refactored into composition
        // this is far more verbose than it needs to be
        case Algorithms.Simplex if config.shouldFbm && config.shouldSeamless =>
          new Simplex(config.seed) with FractalBrownianMotion with SeamlessNoise
        case Algorithms.WhiteNoiseLinear if config.shouldFbm && config.shouldSeamless =>
          new WhiteNoise(config.seed) with LinearInterp with FractalBrownianMotion with SeamlessNoise
        case Algorithms.WhiteNoiseNearest if config.shouldFbm && config.shouldSeamless =>
          new WhiteNoise(config.seed) with NearestNeighborInterp with FractalBrownianMotion with SeamlessNoise

        case Algorithms.Simplex if config.shouldFbm =>
          new Simplex(config.seed) with FractalBrownianMotion
        case Algorithms.WhiteNoiseLinear if config.shouldFbm =>
          new WhiteNoise(config.seed) with LinearInterp with FractalBrownianMotion
        case Algorithms.WhiteNoiseNearest if config.shouldFbm =>
          new WhiteNoise(config.seed) with NearestNeighborInterp with FractalBrownianMotion

        case Algorithms.Simplex if config.shouldSeamless =>
          new Simplex(config.seed) with SeamlessNoise
        case Algorithms.WhiteNoiseLinear if config.shouldSeamless =>
          new WhiteNoise(config.seed) with LinearInterp with SeamlessNoise
        case Algorithms.WhiteNoiseNearest if config.shouldSeamless =>
          new WhiteNoise(config.seed) with NearestNeighborInterp with SeamlessNoise


        case Algorithms.Simplex => new Simplex(config.seed)
        case Algorithms.WhiteNoiseLinear => new WhiteNoise(config.seed) with LinearInterp
        case Algorithms.WhiteNoiseNearest => new WhiteNoise(config.seed) with NearestNeighborInterp
      }

      saveImage(algo,
        config.width, config.height,
        config.scale, config.output)

      sys.exit(1)

    case None => sys.exit(1)
  }
}
