import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.util.Try

package object img {

  trait ImageTransformation[T] {
    def transform(pixels: Picture): T
  }

  object implicits {

    object buffered {

      implicit val asBufferedImage = new ImageTransformation[BufferedImage] {
        override def transform(picture: Picture): BufferedImage = {
          val newImage =
            new BufferedImage(picture.width, picture.height, BufferedImage.TYPE_INT_ARGB)
          TraversablePicture.traverse(picture.pixels) {
            case (pixel, row, col) => newImage.setRGB(row, col, pixel.getRGB)
          }
          newImage
        }
      }

      implicit class BufferedImageOps(image: BufferedImage) {
        def toPng(filename: String): Try[File] = {
          Try {
            val outputfile = new File(filename)
            ImageIO.write(image, "png", outputfile)
            outputfile
          }
        }
      }

    }

  }

}
