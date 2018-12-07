package com.fabian.icarus.img2

object fp {

  import scala.collection.parallel.immutable.ParVector
  case class Image[T](w: Int, h: Int, data: ParVector[T]) {
    def apply(x: Int, y: Int): T = data(x * h + y)

    def map[S](f: T => S): Image[S] = Image(w, h, data map f)

    def updated(x: Int, y: Int, value: T): Image[T] =
      Image(w, h, data.updated(x * h + y, value))

    def as[U](implicit trans: FormatTransformation[U]): U =
      trans.transform[T](this)
  }

  case class PImage[T](x: Int, y: Int, image: Image[T]) {
    def extract: T = image(x, y)

    def map[S](f: T => S): PImage[S] = PImage(x, y, image map f)

    def coflatMap[S](f: PImage[T] => S): PImage[S] = {
      val data = (0 until (image.w * image.h)).toVector.par.map { i =>
        val xx = i / image.h
        val yy = i % image.h
        f(PImage(xx, yy, image))
      }
      PImage(x, y, Image(image.w, image.h, data))
    }

    def up: PImage[T] = {
      val py = y - 1
      val ny = if (py >= 0) py else py + image.h
      PImage(x, ny, image)
    }

    def down: PImage[T] = {
      val py = y + 1
      val ny = if (py < image.h) py else py - image.h
      PImage(x, ny, image)
    }

    def left: PImage[T] = {
      val px = x - 1
      val nx = if (px >= 0) px else px + image.w
      PImage(nx, y, image)
    }

    def right: PImage[T] = {
      val px = x + 1
      val nx = if (px < image.w) px else px - image.w
      PImage(nx, y, image)
    }
  }

}

object filters {

  import cats.{Foldable, NonEmptyParallel}
  import cats.instances.double._
  import cats.syntax.parallel._
  import cats.syntax.foldable._

  def linearFilter[F[_]: Foldable, G[_]](
    weights: F[Double],
    s: F[Double]
  )(implicit ev: NonEmptyParallel[F, G]): Double =
    (weights, s).parMapN(_ * _).fold
}
