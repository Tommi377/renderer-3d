package s1.demo

import javax.sound.sampled._

import s1.image.ImageExtensions._
import java.awt.Color
import java.awt.BasicStroke
import scala.util.Random

import scala.math.{sin, cos, Pi, abs, sqrt, atan, tan, asin, pow}

/*
 * created by: Joonas Niemi
 */
object OffsetStuff extends Effect(500, 500) {

  private var iter = 0
  private val iterEnd = 1000
  private val twoPi = 2 * Pi
  private var changeIntensity = 5
  private var changeDir = -1

  def changeThings(): Unit = {
    this.iter += 1
  }

  private def getIterColor(offset: Int): Color = {
    val ofReal = this.twoPi * ((this.iter * this.changeIntensity * this.changeDir + offset) % 360).toDouble / 360

    val r = (255 * (sin(ofReal + this.twoPi * (1.0 / 3)) / 2 + 0.5)).toInt
    val g = (255 * (sin(ofReal + this.twoPi * (2.0 / 3)) / 2 + 0.5)).toInt
    val b = (255 * (sin(ofReal + this.twoPi * (3.0 / 3)) / 2 + 0.5)).toInt

    new Color(r, g, b, 255)
  }

  def next = this.iter > this.iterEnd

  private def pixelOffsetTighteningLines(x: Int, y: Int): Int = {
    (atan((this.height.toDouble / 2 - y) / (this.width.toDouble / 2 - x)) * this.iter * sqrt(
      this.iter
    )).toInt
  }

  private def pixelOffsetFunkyWunks(x: Int, y: Int): Int = {
    (atan((this.height.toDouble / 2 - y) / (this.width.toDouble / 2 - x)) * this.iter * this.iter / (this.iterEnd / 500)).toInt
  }

  private def pixelOffsetRotatingCircle(x: Int, y: Int): Int = {

    (atan(
      (this.height.toDouble / 2 - y * tan(sqrt((this.iter)))) / (this.width.toDouble / 2 - x * tan(
        sqrt((this.iter))
      ))
    ) * 360).toInt

  }

  private def pixelOffsetCircleOutside(x: Int, y: Int): Int = {
    val a = abs((x - this.width / 2).toDouble / this.width)
    val b = abs((y - this.height / 2).toDouble / this.height)

    (sqrt(a * a + b * b) * 720).toInt
  }

  private def pixelOffsetRandom(x: Int, y: Int): Int = {
    Random.nextInt(
      ((0.5 + sin((this.iter * 5 % 360).toDouble / 360 * this.twoPi) / 2) * 2 * 360 + 1).toInt
    )
  }

  private def pixelOffsetCombo(x: Int, y: Int): Int = {
    val step = (this.width + this.height) / 8
    val fromMiddle = sqrt(
      (pow(this.width / 2 - x, 2) + pow(this.height / 2 - y, 2))
    )

    if (fromMiddle > (((this.iter / 30 + Random.nextInt(3)) % 3 + 1) * step))
      pixelOffsetCircleOutside(x, y)
    else if (fromMiddle > (((this.iter / 30 + Random.nextInt(3)) % 3 + 1) * step))
      pixelOffsetFunkyWunks(x, y)
    else if (fromMiddle > (((this.iter / 30 + Random.nextInt(3)) % 3 + 1) * step))
      pixelOffsetRotatingCircle(x, y)
    else
      pixelOffsetRandom(x, y)
  }

  private def pixelOffset(x: Int, y: Int): Int = {
    (this.iter * 6 / this.iterEnd) match {
      case 0 => pixelOffsetCircleOutside(x, y)
      case 1 => pixelOffsetTighteningLines(x, y)
      case 2 => pixelOffsetFunkyWunks(x, y)
      case 3 => pixelOffsetRotatingCircle(x, y)
      case 4 => pixelOffsetRandom(x, y)
      case _ => pixelOffsetCombo(x, y)
    }
  }

  def makePic() = {

    // Get an empty space where to draw
    val pic = emptyImage

    // Get the tools to draw with
    val graphics = pic.graphics

    val height = pic.getHeight()
    val width = pic.getWidth()

    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val c = getIterColor(pixelOffset(x, y))

      pic.setRGB(x, y, c.getRGB())

    }

    pic
  }
}
