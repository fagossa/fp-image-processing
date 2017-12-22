package img

import java.awt.image.BufferedImage
import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Success

class PictureSpec extends WordSpec with MustMatchers {

  "a picture" should {
    "be loaded from disk" in {
      Picture.fromFile(new File("src/main/resources/bulbasaur.png")) must be('Success)
    }

    "be loaded from disk then transformed back to a file" in {
      import img.implicits.buffered._

      Picture.fromFile(new File("src/main/resources/bulbasaur.png")) match {
        case Success(pic) =>
          pic.as[BufferedImage].toPng("result.png") must be('Success)
        case _ => // do nothing because tested elsewhere
      }
    }
  }

}
