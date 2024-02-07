package s1.demo
import scala.collection.mutable.Buffer
import java.awt.Color

// Test file where you can move freely
object MovementTest extends Engine3D(500, 500, 90, 1000, 0.1) {
    val mesh = Mesh.fromFile("intro", Color.PINK)
    camera.pos = camera.pos + Vector3(0,0,-10)

    meshes += mesh

    override def changeThings(): Unit = {
        //mesh.translate(0,0,-15)
        //mesh.rotateY(2)
        //mesh.translate(0,0,15)
        //mesh.translate(0.2,0,0)
        //mesh.rotate(2,4,6)
        // camera.pos = camera.pos + Vector3(0.1,0,0)
        camera.pointAt(mesh.pos)
    }

    override def keyPressed(key: String) = {
        if (key == "Up") camera.pos = camera.pos + Vector3.up
        if (key == "Down") camera.pos = camera.pos + Vector3.down
        if (key == "Left") camera.pos = camera.pos - camera.direction.matrixMultiply(Matrix4.yRotationMatrix(Math.toRadians(90)))
        if (key == "Right") camera.pos = camera.pos + camera.direction.matrixMultiply(Matrix4.yRotationMatrix(Math.toRadians(90)))
        if (key == "W") camera.pos = camera.pos + camera.direction
        if (key == "S") camera.pos = camera.pos - camera.direction
        if (key == "A") camera.yaw = camera.yaw - 10
        if (key == "D") camera.yaw = camera.yaw + 10
        if (key == "Q") camera.roll = camera.roll + 10 // Experimental
        if (key == "E") camera.roll = camera.roll - 10 // Experimental
        if (key == "R") camera.pitch = camera.pitch + 10
        if (key == "F") camera.pitch = camera.pitch - 10
        if (key == "Space") toggleWireframe()
    }
}