package img.executing

import java.awt.image.BufferedImage
import java.io.File

import img.Picture

import scala.util.{Failure, Success, Try}

object PictureExecutor extends App {

  import img.implicits.buffered._

  Picture.fromFile(new File("src/main/resources/bulbasaur.png")) match {
    case Success(pic) =>
      println("No problemo loading image")
      val result: Try[File] = pic.as[BufferedImage].toPng("result.png")
      println(result)

    case Failure(err) =>
      Console.err.println(err)
  }
}
