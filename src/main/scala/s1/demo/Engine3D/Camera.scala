package s1.demo

class Camera(var pos: Vector3 = Vector3.origin, var direction: Vector3 = Vector3.forward) {
    var yaw = 0.0
    var pitch = 0.0
    var roll = 0.0  // Experimental AKA works oddly

    def viewMatrix() = {
        pitch = pitch.max(-89).min(89)

        val yRot = Matrix4.yRotationMatrix(Math.toRadians(this.yaw))
        val xRot = Matrix4.xRotationMatrix(Math.toRadians(this.pitch))
        val zRot = Matrix4.zRotationMatrix(Math.toRadians(this.roll))

        val cameraRotationMatrix = xRot * yRot
        val cameraUp = Vector3.up.matrixMultiply(zRot)
        direction = Vector3.forward.matrixMultiply(cameraRotationMatrix)
        val cameraTarget = direction + this.pos
        val cameraMatrix = Matrix4.pointAtMatrix(this.pos, cameraTarget, cameraUp)
        cameraMatrix.inverse
    }

    def pointAt(otherPos: Vector3) = {
        val vec = otherPos - pos
        yaw = Math.signum(pos.x)*Math.toDegrees(Math.acos(vec.z / Math.sqrt(vec.x * vec.x + vec.z * vec.z))) // Yaw: xz-plane
        pitch = -Math.signum(pos.y)*Math.toDegrees(Math.acos(vec.z / Math.sqrt(vec.y * vec.y + vec.z * vec.z))) // Pitch: yz-plane
    }
}