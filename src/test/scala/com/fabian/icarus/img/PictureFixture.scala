package com.fabian.icarus.img

import scala.util.Try

trait PictureFixture {

  class ImageBuilder {
    def filledWith(aContent: Matrix[String]): Try[Picture[String]] =
      Try(new Picture[String](aContent))
  }

  def aStringImage = new ImageBuilder

  def anImageFunctorFrom(rawContent: String): Picture[String] = {
    val contents: Matrix[String] = rawContent
      .split("\n")
      .map(_.toCharArray.toVector.map(_.toString))
      .toVector
      .filter(_.map(_.trim).mkString.nonEmpty)
    aStringImage.filledWith(contents).get
  }

}
