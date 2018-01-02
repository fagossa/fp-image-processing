package com.fabian.icarus.img

class Picture[U](val pixels: Matrix[U]) {

  import TraversablePicture._

  def height: Int = pixels.length

  def width: Int = pixels(0).length

  def pixelByRowColumn(row: Int, column: Int): U =
    pixels(row)(column)

  def flipInVerticalAxis: Picture[U] = {
    val newPixels: Matrix[U] = traverseMap(pixels) {
      case (data, row) =>
        traverseMap[U, U](data) {
          case (_, column) => pixelByRowColumn(row, width - 1 - column)
        }
    }
    new Picture(newPixels)
  }

  def flipInHorizontalAxis: Picture[U] = {
    val newPixels: Matrix[U] = traverseMap(pixels) {
      case (data, row) =>
        traverseMap[U, U](data) {
          case (_, column) => pixelByRowColumn(height - 1 - row, column)
        }
    }
    new Picture(newPixels)
  }

  def as[T](implicit trans: FormatTransformation[U, T]): T =
    trans.transform(this)

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
