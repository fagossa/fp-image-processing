package com.fabian.icarus

import java.awt.image.BufferedImage
import java.io.File

import com.fabian.icarus.img.PictureBuilder

import scala.util.{Failure, Success, Try}

object StartIcarus extends App {

  import com.fabian.icarus.img.implicits.buffered._

  PictureBuilder.fromFile(new File("src/main/resources/bulbasaur.png")) match {
    case Success(pic) =>
      println("No problemo loading image")
      val result: Try[File] = pic.as[BufferedImage].toPng("result.png")
      println(result)

    case Failure(err) =>
      Console.err.println(err)
  }
}
