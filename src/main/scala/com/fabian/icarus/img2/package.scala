package com.fabian.icarus

import com.fabian.icarus.img2.fp.Image

package object img2 {
  trait FormatTransformation[U] {
    def transform[T](image: Image[T]): U
  }
}
