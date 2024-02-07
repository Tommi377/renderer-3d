package s1.demo
import scala.collection.mutable.{Buffer, Queue}
import java.awt.Color

object TitleAnimation extends Engine3D(500, 500, 90, 1000, 0.1) {
    // Initial location
    val startLoc = Location(Vector3(-55,17,38), 20, -40)
    val tommiP = Location(Vector3(2,18,29), 240, -40)
    val joonasN = Location(Vector3(50,21,40), 280, -50)
    val tommiK = Location(Vector3(55,13,-25), 150, -30)
    val ilariT = Location(Vector3(-37,16,-50), 60, -30)
    val endLoc = Location(Vector3(-67,17,47), 0, -40)

    val locOrder = Queue(tommiP, joonasN, tommiK, ilariT, startLoc)



    camera.pos = startLoc.pos
    camera.yaw = startLoc.yaw
    camera.pitch = startLoc.pitch

    var end = false

    var lerpStart = startLoc
    var lerpEnd = startLoc
    var lerpPer = 1.0
    var lerpInc = 0.0

    var sleep = 0
    var tikFlag = false

    val area = Mesh.fromFile("intro")
    meshes += area

    val tik = Mesh.fromFile("tik")
    tik.position = Vector3(-57, 10, 45)
    tik.rotation = Rotation(35,20,0)

    def newTarget(loc: Location, dur: Double): Unit = {
        lerpStart = Location(camera.pos, camera.yaw, camera.pitch)
        lerpEnd = loc
        lerpPer = 0.0
        lerpInc = 1/dur
    }

    override def next = end

    override def changeThings(): Unit = {
        if (lerpPer < 1) {
            lerpPer = (lerpPer + lerpInc).min(1)
            val posInc = (lerpEnd.pos - lerpStart.pos) * lerpPer
            
            camera.pos = lerpStart.pos + posInc
            var yawDiff = (lerpEnd.yaw - lerpStart.yaw)
            var pitchDiff = (lerpEnd.pitch - lerpStart.pitch)
            if (math.abs(yawDiff) > 180) {
                yawDiff = math.signum(yawDiff)*360 - math.abs(yawDiff)
            }
            if (math.abs(pitchDiff) > 180) {
                pitchDiff = math.signum(pitchDiff)*360 - math.abs(pitchDiff)
            }
            camera.yaw -= yawDiff * lerpInc
            camera.pitch += pitchDiff * lerpInc
        } else {
            sleep += 1
            if (sleep > 16) {
                if (locOrder.length == 0) end = true
                else {
                    sleep = 0
                    newTarget(locOrder.dequeue, 32)
                    if (locOrder.length == 0 && !tikFlag) {
                        tikFlag = true
                        meshes += tik
                    }
                }
            }
        }
    }
}

case class Location(pos: Vector3, yaw: Double, pitch: Double)