package com.fabian.picture.untyped.executing

import java.io.File

import com.fabian.picture.untyped.Picture

import scala.util.{Failure, Success}

object PictureExecutor extends App {
  Picture.fromFile(new File("src/main/resources/bulbasaur.png")) match {
    case Success(pic) =>
      println("No problemo loading image")

    case Failure(err) =>
      Console.err.println(err)
  }
}
