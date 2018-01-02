package com.fabian.icarus.img

import java.awt.image.BufferedImage
import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

class PictureSpec extends WordSpec with MustMatchers {

  "a picture" must {
    val anImage = PictureBuilder.fromFile(new File("src/main/resources/bulbasaur.png"))

    "be loaded from disk" in {
      anImage must be('Success)
    }

    "be loaded from disk then transformed back to a file" in {
      import com.fabian.icarus.img.implicits.buffered._
      anImage.foreach { pic =>
        pic.as[BufferedImage].toPng("result.png") must be('Success)
      }
    }

    "be flipped vertically" in {
      import com.fabian.icarus.img.implicits.buffered._
      anImage.foreach { pic =>
        pic.flipInHorizontalAxis.as[BufferedImage].toPng("result.png") must be('Success)
      }
    }
  }

}
