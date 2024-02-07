package s1.demo

import scala.swing.SimpleSwingApplication
import scala.swing.Swing._
import scala.swing.MainFrame
import scala.collection.mutable.Buffer
import scala.util.Random

import java.awt.BasicStroke
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics

import math._
import s1.image.ImageExtensions._

object TrippyTeapot extends Engine3D(500, 500, 90, 1000, 0.1) {
    var step = 0
    var sleep = 64
    var end = false

    var d = 0.0 // S0
    var s1cycle = 0.0 // S1
    var fillPer = 0.0 // S1
    var circleRadius = 0.0 // S2
    var s2cycle = -Pi / 2 // S2
    var miniRadius = 0.0 // S3
    var miniOffset = 0.0 // S4
    var s3cycle = -Pi / 2 // S2

    var currentColor = Color.WHITE
    
    val teapot = Mesh.fromFile("teapot")
    meshes += teapot
    camera.pos = camera.pos + Vector3(0,0,-4)

    light = Vector3.backward
    this.toggleWireframe()

    def advanceStep() = {
        step += 1
        step match {
            case 1 => if (this.wireframe) this.toggleWireframe()
            case _ => Unit
        }
    }

    def posInCircle(x: Double, y: Double, x0: Double, y0: Double) = sqrt(pow(x - x0, 2) + pow(y - y0, 2))

    override def changeThings(): Unit = {
        d += 0.1

        currentColor = new Color(
            ((sin(d) + 1) / 2 * 255).toInt,
            ((sin(Pi/16*d) + 1) / 2 * 255).toInt,
            ((sin(Pi/8*d) + 1) / 2 * 255).toInt
        )
        bgColor = new Color(
            ((sin(Pi/32*d) + 1) / 2 * 255).toInt,
            ((sin(Pi/8*d) + 1) / 2 * 255).toInt,
            ((sin(Pi/16*d) + 1) / 2 * 255).toInt
        )

        wireframeColor = currentColor
        lightColor = currentColor

        teapot.rotate(2, 5, 2)

        if (step >= 1) {
            fillPer += 0.01
            s1cycle += height / 32.0
        }
        if (step >= 2) {
            s2cycle += (Pi / 64)
            circleRadius = (sin(s2cycle) + 1) * max(width, height) / 4
        }
        if (step >= 3) miniRadius = miniRadius + max(width, height) / 128.0
        if (step >= 4) {
            s3cycle += (Pi / 32)
            miniOffset = (sin(s3cycle) + 1) * max(width, height) / 4
        }
    }

    override def next = end
    
    override def makePic(): BufferedImage = {
        val pic = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        val g = pic.graphics

        g.setColor(bgColor)
        g.fillRect(0, 0, pic.getWidth, pic.getHeight)

        makeBackground(g)

        val points = getProjectedLines()

        for (triangle <- points) {
            g.setStroke(new BasicStroke(1))
            val (x, y) = getTriangleLines(triangle)
            g.setColor(triangle.color)
            g.drawPolygon(x, y, 3)
            if ((step == 1 && Random.nextDouble() < fillPer) || (step != 1 && !wireframe)) {
                g.fillPolygon(x, y, 3)
            }
        }

        for {
            x <- 0 until width
            y <- 0 until height
        } {
            var c = new Color(pic.getRGB(x, y))
            if (c.getRGB != bgColor.getRGB && c.getRGB != new Color(255 - bgColor.getRed, 255 - bgColor.getGreen, 255 - bgColor.getBlue)) {
                if (step >= 2) {
                    if (posInCircle(x, y, width / 2, height / 2) < circleRadius) {
                        c = new Color(255 - c.getRed, 255 - c.getGreen, 255 - c.getBlue)
                    }
                }

                if (step >= 3) {
                    if (posInCircle(x, y, width/4 + miniOffset, height/4) % (width / 4) < miniRadius % 128) c = new Color(c.getBlue, c.getRed, c.getGreen)
                    if (posInCircle(x, y, 3*width/4, height/4 + miniOffset) % (width / 4) < miniRadius % 128) c = new Color(c.getGreen,c.getBlue, c.getRed)
                    if (posInCircle(x, y, width/4, 3*height/4 - miniOffset) % (width / 4) < miniRadius % 128) c = new Color(255 - c.getBlue, 255 - c.getRed, 255 - c.getGreen)
                    if (posInCircle(x, y, 3*width/4 - miniOffset, 3*height/4) % (width / 4) < miniRadius % 128) c = new Color(255 - c.getGreen, 255 - c.getBlue, 255 - c.getRed)
                }
            } else {
                if (y < s1cycle && ((y + (s1cycle / 32 % 128)) / 24) % 2 < 1) {
                    c = new Color(255 - c.getRed, 255 - c.getGreen, 255 - c.getBlue)
                }
            }
            pic.setRGB(x, y, c.getRGB)
        }
        sleep += 1
        if (sleep > 128) {
            if (step == 4) {
                end = true
            } else {
                sleep = 0
                advanceStep()
            }
        }

        pic
    }
}