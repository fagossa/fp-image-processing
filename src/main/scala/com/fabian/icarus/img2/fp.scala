package com.fabian.icarus.img2

object fp {

  import scala.collection.parallel.immutable.ParVector
  case class Image[T](w: Int, h: Int, data: ParVector[T]) {

    def apply(x: Int, y: Int): T = data(x * h + y)

    def map[S](f: T => S): Image[S] = Image(w, h, data map f)

    def updated(x: Int, y: Int, value: T): Image[T] =
      Image(w, h, data.updated(x * h + y, value))
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
