package com.fabian.icarus.img

import java.io.File

import scala.util.Try

trait PictureBuilder[T] {
  def build(imageFile: File): Try[Picture[T]]
}

object PictureBuilder {

  def fromFile[T](imageFile: File)(implicit pictureBuilder: PictureBuilder[T]) =
    pictureBuilder.build(imageFile)

  import java.awt.Color
  implicit val colorPictureBuilder = new PictureBuilder[Color] {
    import java.awt.image.BufferedImage
    import java.io.File
    import javax.imageio.ImageIO

    override def build(imageFile: File): Try[Picture[Color]] =
      Try {
        val pixels: Matrix[Color] = buildPixelMatrixFrom(ImageIO.read(imageFile))
        new Picture(pixels)
      }

    private def buildPixelMatrixFrom(image: BufferedImage): Matrix[Color] = {
      import TraversablePicture._
      val width = image.getWidth
      val height = image.getHeight
      traverseMap(Vector.fill(height, width)(Color.BLACK)) {
        case (data, column) =>
          traverseMap[Color, Color](data) {
            case (_, row) => buildPixel(image, row, column)
          }
      }
    }

    private def buildPixel(image: BufferedImage, row: Int, column: Int): Color = {
      var pixel = new Array[Int](4) // RGB + alpha
      image.getData.getPixel(row, column, pixel)
      new Color(
        pixel(0), // R
        pixel(1), // G
        pixel(2) // B
      )
    }
  }

}
