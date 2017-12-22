package img

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.util.Try

case class Picture(pixels: Matrix[Color]) {

  import TraversablePicture._

  def height: Int = pixels.length

  def width: Int = pixels(0).length

  def pixelByRowColumn(row: Int, column: Int): Color = pixels(row)(column)

  def flipInVerticalAxis(): Picture = {
    val newPixels: Matrix[Color] = traverseMap(pixels) {
      case (data, row) =>
        traverseMap[Color, Color](data) {
          case (_, column) => pixelByRowColumn(row, width - 1 - column)
        }
    }
    Picture(newPixels)
  }

  def flipInHorizontalAxis(): Picture = {
    /*val pixels = new Vector[Vector[Color]](height)(width).map {
      row =>
        new Vector[Color](width) {
          column =>
            pixelByRowColumn(height - 1 - row, column)
        }
    }
    Picture(pixels)
     */
    this
  }

  def as[T](implicit trans: FormatTransformation[T]) = trans.transform(this)

}

object TraversablePicture {

  def traverseMap[U, V](vector: Vector[U])(f: (U, Int) => V): Vector[V] = {
    vector.zipWithIndex.map {
      case (t, pos: Int) => f(t, pos)
    }
  }

  def traverseMap2[U, V](vector: Matrix[U])(f: (U, Int, Int) => V): Matrix[V] = {
    traverseMap[Vector[U], Vector[V]](vector) {
      case (data, column) =>
        traverseMap[U, V](data) {
          case (pixel, row) => f(pixel, row, column)
        }
    }
  }

  def traverse[U](vector: Matrix[U])(f: (U, Int, Int) => Unit): Unit = {
    traverseMap[Vector[U], Unit](vector) {
      case (data, column) =>
        traverseMap[U, Unit](data) {
          case (pixel, row) => f(pixel, row, column)
        }
    }
  }

}

object Picture {

  def fromFile(imageFile: File): Try[Picture] = {
    Try {
      val pixels: Matrix[Color] = buildPixelMatrixFrom(ImageIO.read(imageFile))
      Picture(pixels)
    }
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
