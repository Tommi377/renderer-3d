package s1.demo

import scala.collection.mutable.{Buffer, Map}
import scala.io.Source
import java.awt.Color

object Vector3 {
    def up = Vector3(0, 1, 0)
    def down = Vector3(0, -1, 0)
    def left = Vector3(1, 0, 0)
    def right = Vector3(-1, 0, 0)
    def forward = Vector3(0, 0, 1)
    def backward = Vector3(0, 0, -1)
    def origin = Vector3(0, 0, 0)
}

case class Vector3(x: Double, y: Double, z: Double) {
    def normalized = Vector3(x / this.length, y / this.length, z / this.length)
    def length = Math.sqrt(this * this)
    def crossProduct(other: Vector3) = Vector3(
        this.y * other.z - this.z * other.y,
        this.z * other.x - this.x * other.z,
        this.x * other.y - this.y * other.x
    )
    def matrixMultiply(m: Matrix4): Vector3 = {
        val x = this.x * m(0)(0) + this.y * m(1)(0) + this.z * m(2)(0) + m(3)(0)
        val y = this.x * m(0)(1) + this.y * m(1)(1) + this.z * m(2)(1) + m(3)(1)
        val z = this.x * m(0)(2) + this.y * m(1)(2) + this.z * m(2)(2) + m(3)(2)
        var s = this.x * m(0)(3) + this.y * m(1)(3) + this.z * m(2)(3) + m(3)(3)
        if (s == 0.0) s = 1
        Vector3(x / s, y / s, z / s)
    }

    def + (other: Vector3) = Vector3(this.x + other.x, this.y + other.y, this.z + other.z)
    def - (other: Vector3) = Vector3(this.x - other.x, this.y - other.y, this.z - other.z)
    def * (other: Vector3) = this.x * other.x + this.y * other.y + this.z * other.z
    def * (num: Double) = Vector3(this.x * num, this.y * num, this.z * num)
    def / (num: Double) = Vector3(this.x / num, this.y / num, this.z / num)

    override def toString = s"($x, $y, $z)"
}

// Clockwise
object Triangle {
    def apply(p1: Vector3, p2: Vector3, p3: Vector3): Triangle = new Triangle((p1, p2, p3))
    def apply(p1: Vector3, p2: Vector3, p3: Vector3, color: Color): Triangle = new Triangle((p1, p2, p3), color)
    def apply(points: (Vector3, Vector3, Vector3)): Triangle = new Triangle(points)
    def apply(points: (Vector3, Vector3, Vector3), color: Color): Triangle = new Triangle(points, color)
    def apply(points: Vector[Vector3]): Triangle = new Triangle((points(0), points(1), points(2)))
    def apply(points: Vector[Vector3], color: Color): Triangle = new Triangle((points(0), points(1), points(2)), color)
}

class Triangle(points: (Vector3, Vector3, Vector3), var color: Color = Color.WHITE) {
    var p = Array(points._1, points._2, points._3)

    def normal = {
        val line1 = Vector3(p(1).x - p(0).x, p(1).y - p(0).y, p(1).z - p(0).z)
        val line2 = Vector3(p(2).x - p(0).x, p(2).y - p(0).y, p(2).z - p(0).z)
        
        line1.crossProduct(line2).normalized
    }


    def clipTriangle(planePoint: Vector3, planeNormal: Vector3): Vector[Triangle]= {
        def intersectionPoint(start: Vector3, end: Vector3) = {
            val planeDot = planePoint * planeNormal
            val startDot = start * planeNormal
            val endDot = end * planeNormal
            val intersectFactor = (planeDot - startDot) / (endDot - startDot)
            val line = end - start
            line * intersectFactor + start
        }
        def distanceToPlane(point: Vector3) = {
            val pointNormalized = point.normalized
            (planeNormal * pointNormalized) - (planeNormal * planePoint)
        }

        val inside: Buffer[Vector3] = Buffer()
        val outside: Buffer[Vector3] = Buffer()

        for (i <- 0 until 3) {
            if (distanceToPlane(this.p(i)) >= 0) inside += this.p(i)
            else outside += this.p(i)
        }

        inside.length match {
            case 0 => Vector() // Do not render
            case 1 => Vector(Triangle(inside(0), intersectionPoint(inside(0), outside(0)), intersectionPoint(inside(0), outside(1)), this.color))
            case 2 => {
                val tri1 = Triangle(inside(0), inside(1), intersectionPoint(inside(0), outside(0)), this.color)
                val tri2 = Triangle(inside(1), intersectionPoint(inside(0), outside(0)), intersectionPoint(inside(1), outside(0)), this.color)
                Vector(tri1, tri2)
            }
            case 3 => Vector(this) // No need to clip
            case _ => Vector() // Shouldn't happen
        }
    }

    override def toString = s"1: ${p(0)}, 2: ${p(1)}, 3: ${p(2)}"
}

class Mesh(var tris: Vector[Triangle], var position: Vector3 = Vector3.origin, var rotation: Rotation = Rotation(0,0,0)) {
    var offset = Vector3.origin // Offset the mesh's center point

    def pos = position + offset

    def translate(x: Double, y: Double, z: Double): Unit = this.translate(Vector3(x,y,z))
    def translate(vec: Vector3): Unit = {
        val translationMatrix = Matrix4.translationMatrix(vec.x, vec.y, vec.z)
        this.position = this.position.matrixMultiply(translationMatrix)
    }

    def rotate(x: Double, y: Double, z: Double): Unit = this.rotate(Rotation(x, y, z))
    def rotate(rot: Rotation): Unit = {
        this.rotation = this.rotation + rot
    }

    def rotateX(angle: Double) = {
        val radian = Math.toRadians(angle)
        val rotationMatrix = Matrix4.xRotationMatrix(radian)
        this.tris.foreach(tri => tri.p = tri.p.map(vec => (vec + offset).matrixMultiply(rotationMatrix)))
    }

    def rotateY(angle: Double) = {
        val radian = Math.toRadians(angle)
        val rotationMatrix = Matrix4.yRotationMatrix(radian)
        this.tris.foreach(tri => tri.p = tri.p.map(vec => (vec + offset).matrixMultiply(rotationMatrix)))
    }

    def rotateZ(angle: Double) = {
        val radian = Math.toRadians(angle)
        val rotationMatrix = Matrix4.zRotationMatrix(radian)
        this.tris.foreach(tri => tri.p = tri.p.map(vec => (vec + offset).matrixMultiply(rotationMatrix)))
    }
}

object Mesh {
    def fromFile(name: String, defaultColor: Color = Color.WHITE): Mesh = {
        val combinedTris = fromFileVector(name, defaultColor).map(_.tris).flatten
        new Mesh(combinedTris)
    }

    def fromFileVector(name: String, defaultColor: Color = Color.WHITE): Vector[Mesh] = {
        val objStream = getClass.getResourceAsStream(s"/models/$name.obj")
        val mtlStream = getClass.getResourceAsStream(s"/models/$name.mtl")

        val colorMap: Map[String, Color] = Map()
        
        val vertexBuffer: Buffer[Vector3] = Buffer()
        var triangleBuffer: Buffer[Triangle] = Buffer()
        var meshBuffer: Buffer[Mesh] = Buffer()

        try {
            var mtl = ""
            Source.fromInputStream(mtlStream).getLines.foreach((line: String) => {
                val split = line.split(" ")
                if (line.length > 0) {
                    split(0) match {
                        case "newmtl" => mtl = split(1)
                        case "Kd" => {
                            val r = (split(1).toDouble * 255).toInt
                            val g = (split(2).toDouble * 255).toInt
                            val b = (split(3).toDouble * 255).toInt
                            colorMap += mtl -> new Color(r, g, b)
                        }
                        case _ => Unit
                    }
                }
            })
        } catch {
            case e: NullPointerException => println("No mtl file")
            case e: Throwable => println(e)
        }
        
        var currentColor = defaultColor
        Source.fromInputStream(objStream).getLines.foreach((line: String) => {
            if (line.length > 0) {
                try {
                    val split = line.split(" ")
                    split(0) match {
                        case "o" => {
                            if (triangleBuffer.length > 0) {
                                currentColor = defaultColor
                                meshBuffer += new Mesh(triangleBuffer.toVector)
                                triangleBuffer = Buffer()
                            }
                        }
                        case "v" => vertexBuffer += new Vector3(split(1).split("/")(0).toDouble, split(2).split("/")(0).toDouble, split(3).split("/")(0).toDouble)
                        case "usemtl" => {
                            val colorOption = colorMap.lift(split(1))
                            if (colorOption.isDefined) currentColor = colorOption.get
                        }
                        case "f" => triangleBuffer += new Triangle((vertexBuffer(split(1).split("/")(0).toInt - 1), vertexBuffer(split(2).split("/")(0).toInt - 1), vertexBuffer(split(3).split("/")(0).toInt - 1)), currentColor)
                        case _ => Unit
                    }
                } catch {
                    case e: Throwable => println(e)
                }
            }
        })
        meshBuffer += new Mesh(triangleBuffer.toVector)
        meshBuffer.toVector
    }

    def createCube(p1: Vector3, p2: Vector3): Mesh = {
        new Mesh(Vector(
            // North
            Triangle((Vector3(p1.x, p1.y, p1.z), Vector3(p1.x, p2.y, p1.z), Vector3(p2.x, p2.y, p1.z))),
            Triangle((Vector3(p2.x, p2.y, p1.z), Vector3(p2.x, p1.y, p1.z), Vector3(p1.x, p1.y, p1.z))),

            // South
            Triangle((Vector3(p2.x, p2.y, p2.z), Vector3(p1.x, p1.y, p2.z), Vector3(p2.x, p1.y, p2.z))),
            Triangle((Vector3(p1.x, p1.y, p2.z), Vector3(p2.x, p2.y, p2.z), Vector3(p1.x, p2.y, p2.z))),

            // West
            Triangle((Vector3(p2.x, p2.y, p2.z), Vector3(p2.x, p1.y, p2.z), Vector3(p2.x, p1.y, p1.z))),
            Triangle((Vector3(p2.x, p1.y, p1.z), Vector3(p2.x, p2.y, p1.z), Vector3(p2.x, p2.y, p2.z))),

            // East
            Triangle((Vector3(p1.x, p1.y, p1.z), Vector3(p1.x, p1.y, p2.z), Vector3(p1.x, p2.y, p2.z))),
            Triangle((Vector3(p1.x, p2.y, p2.z), Vector3(p1.x, p2.y, p1.z), Vector3(p1.x, p1.y, p1.z))),

            // Top
            Triangle((Vector3(p2.x, p2.y, p2.z), Vector3(p2.x, p2.y, p1.z), Vector3(p1.x, p2.y, p1.z))),
            Triangle((Vector3(p1.x, p2.y, p1.z), Vector3(p1.x, p2.y, p2.z), Vector3(p2.x, p2.y, p2.z))),

            // Bottom
            Triangle((Vector3(p1.x, p1.y, p1.z), Vector3(p2.x, p1.y, p1.z), Vector3(p2.x, p1.y, p2.z))),
            Triangle((Vector3(p2.x, p1.y, p2.z), Vector3(p1.x, p1.y, p2.z), Vector3(p1.x, p1.y, p1.z)))
        ))
    }
}

case class Rotation(var x: Double, var y: Double, var z: Double) {
    x = x % 360
    y = y % 360
    z = z % 360

    def +(other: Rotation) = Rotation(this.x + other.x, this.y + other.y, this.z + other.z)
    def -(other: Rotation) = Rotation(this.x - other.x, this.y - other.y, this.z - other.z)
    def *(other: Rotation) = Rotation(this.x * other.x, this.y * other.y, this.z * other.z)
    def /(other: Rotation) = Rotation(this.x / other.x, this.y / other.y, this.z / other.z)

    def toRadians = Rotation(Math.toRadians(x), Math.toRadians(y), Math.toRadians(z))
    override def toString = s"($x, $y, $z)"
}