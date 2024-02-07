package s1.demo

import scala.collection.mutable.{Buffer, Queue}
import java.awt.Color._
import java.awt.BasicStroke
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics

import s1.image.ImageExtensions._

abstract class Engine3D(width: Int, height: Int, val fov: Double, val zFar: Double, val zNear: Double) extends Effect(width, height) {
    var wireframe = false

    val aspectRatio = width / height.toDouble
    val projectionMatrix = Matrix4.projectionMatrix(aspectRatio, fov, zFar, zNear)

    // COLORS
    var bgColor = WHITE
    var lightColor = BLACK
    var wireframeColor = BLACK

    var light = Vector3(0, 1, 0).normalized
    var meshes: Buffer[Mesh] = Buffer()
    var camera = new Camera


    def getProjectedLines() = {
        val projected: Buffer[Triangle] = Buffer()
        val result: Buffer[Triangle] = Buffer()
        val viewMatrix = camera.viewMatrix

        // Pipeline
        // Rotate -> Translate -> Convert view space -> Project to 2D space
        for {
            mesh <- meshes
            tri <- mesh.tris
        } {
            val rotationMatrix = Matrix4.rotationMatrix(mesh.rotation.toRadians)
            val translationMatrix = Matrix4.translationMatrix(mesh.pos)
            var transformedTri = Triangle((for (i <- 0 until 3) yield tri.p(i).matrixMultiply(rotationMatrix).matrixMultiply(translationMatrix)).toVector, tri.color)

            val cameraRay = transformedTri.p(0) - camera.pos

            if (transformedTri.normal * cameraRay < 0) {

                // Lighting
                val dp = (transformedTri.normal.normalized * light).max(0).min(1)
                val color = new Color(
                    ((transformedTri.color.getRed + lightColor.getRed) / 2.0 * dp).toInt,
                    ((transformedTri.color.getGreen + lightColor.getGreen) / 2.0 * dp).toInt,
                    ((transformedTri.color.getBlue + lightColor.getBlue) / 2.0 * dp).toInt
                )

                val triView = Triangle((for (i <- 0 until 3) yield transformedTri.p(i).matrixMultiply(viewMatrix)).toVector)

                triView.clipTriangle(Vector3(0, 0, zNear), Vector3.forward).foreach(_tri => {
                    val triProjected = Triangle((
                        for (i <- 0 until 3)
                        yield {
                            val newVector = _tri.p(i).matrixMultiply(projectionMatrix)
                            Vector3((newVector.x * -1 + 1) * width * 0.5, (newVector.y * -1 + 1) * height * 0.5, newVector.z)
                    }).toVector, color)
    
                    projected += triProjected
                })
            }
        }
        // Painters algorithm and clip triangles against view planes
        projected.sortBy(tri => -(tri.p(0).z + tri.p(1).z + tri.p(2).z) / 3).foreach(triangle => {
            val triList = Queue(triangle)
            var newTris = 1

            for (i <- 0 until 4) {
                while (newTris > 0) {
                    val tri = triList.dequeue()
                    newTris -= 1
                    triList ++= (i match {
                        case 0 => tri.clipTriangle(Vector3(0, 0, 0), Vector3.up)
                        case 1 => tri.clipTriangle(Vector3(0, height - 1, 0), Vector3.down)
                        case 2 => tri.clipTriangle(Vector3(0, 0, 0), Vector3.left)
                        case 3 => tri.clipTriangle(Vector3(width - 1, 0, 0), Vector3.right)
                    })
                }
                newTris = triList.length
            }
            result ++= triList
        })
        result
    }

    def toggleWireframe() = this.wireframe = !this.wireframe

    def getTriangleLines(tri: Triangle): (Array[Int], Array[Int]) = getTriangleLines(tri.p.toVector)
    def getTriangleLines(tri: Vector[Vector3]): (Array[Int], Array[Int]) = (tri.map(_.x.toInt).toArray, tri.map(_.y.toInt).toArray)

    def makePic(): BufferedImage = {
        val pic = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        val g = pic.graphics

        g.setColor(bgColor)
        g.fillRect(0, 0, pic.getWidth, pic.getHeight)

        makeBackground(g)

        val points = getProjectedLines()

        for (triangle <- points) {
            g.setStroke(new BasicStroke(1))
            val (x, y) = getTriangleLines(triangle)
            if (!wireframe) {
                g.setColor(triangle.color)
                g.drawPolygon(x, y, 3)
                g.fillPolygon(x, y, 3)
            } else {
                g.setColor(wireframeColor)
                g.drawPolygon(x, y, 3)
            }
        }

        makeForeground(g)

        pic
    }

    // Override these in the child class if you want to affect the back-/foreground
    def makeBackground(g: Graphics) = Unit
    def makeForeground(g: Graphics) = Unit

    def next = false
}