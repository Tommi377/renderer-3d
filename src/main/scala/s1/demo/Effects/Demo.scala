package s1.demo

import java.awt.Color


object Demo extends Engine3D(500, 500, 90, 1000, 0.1) {
  val mesh = Mesh.fromFile("container")
  meshes += mesh

  camera.pos = camera.pos + Vector3(0,0,-4)
  light = Vector3(-1, 1, -1)

  def changeThings() {

  }

  override def keyPressed(key: String) = {
    if (key == "Up") camera.pos = camera.pos + Vector3.up
    if (key == "Down") camera.pos = camera.pos + Vector3.down
    if (key == "Left") camera.pos = camera.pos + Vector3.left
    if (key == "Right") camera.pos = camera.pos + Vector3.right
    if (key == "W") camera.pos = camera.pos + camera.direction
    if (key == "S") camera.pos = camera.pos - camera.direction
    if (key == "A") camera.yaw = camera.yaw - 10
    if (key == "D") camera.yaw = camera.yaw + 10
    if (key == "Q") camera.roll = camera.roll + 10 // Experimental
    if (key == "E") camera.roll = camera.roll - 10 // Experimental
    if (key == "R") camera.pitch = camera.pitch + 10
    if (key == "F") camera.pitch = camera.pitch - 10
  }

  override def next = false
}