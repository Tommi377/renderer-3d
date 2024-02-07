package s1.demo

class Matrix4(val m: Array[Array[Double]]) {
    def apply(index: Int) = m(index)

    def inverse = {
        val matrix = Array(
            Array(m(0)(0), m(1)(0), m(2)(0), 0.0),
            Array(m(0)(1), m(1)(1), m(2)(1), 0.0),
            Array(m(0)(2), m(1)(2), m(2)(2), 0.0),
            Array(0.0, 0.0, 0.0, 1.0)
        )
		matrix(3)(0) = -(m(3)(0) * matrix(0)(0) + m(3)(1) * matrix(1)(0) + m(3)(2) * matrix(2)(0));
		matrix(3)(1) = -(m(3)(0) * matrix(0)(1) + m(3)(1) * matrix(1)(1) + m(3)(2) * matrix(2)(1));
		matrix(3)(2) = -(m(3)(0) * matrix(0)(2) + m(3)(1) * matrix(1)(2) + m(3)(2) * matrix(2)(2));
        new Matrix4(matrix)
    }

    def *(other: Matrix4) = {
        val matrix = Array.fill(4, 4)(0.0)
        for {
            m <- 0 until 4
            n <- 0 until 4
        } {
            matrix(n)(m) = this(n)(0) * other(0)(m) + this(n)(1) * other(1)(m) + this(n)(2) * other(2)(m) + this(n)(3) * this(3)(m)
        }
        new Matrix4(matrix)
    }
}

object Matrix4 {
    def identityMatrix = {
        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = 1
        matrix(1)(1) = 1
        matrix(2)(2) = 1
        matrix(3)(3) = 1
        new Matrix4(matrix)
    }

    def projectionMatrix(aspectRatio: Double, fov: Double, zFar: Double, zNear: Double) = {
        val fovRadian = 1 / Math.tan(Math.toRadians(fov) * 0.5)

        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = aspectRatio * fovRadian
        matrix(1)(1) = fovRadian
        matrix(2)(2) = zFar / (zFar - zNear)
        matrix(3)(2) = (-zFar * zNear) / (zFar - zNear)
        matrix(2)(3) = 1.0
        new Matrix4(matrix)
    }

    def translationMatrix(vec: Vector3): Matrix4 = translationMatrix(vec.x, vec.y, vec.z)
    def translationMatrix(x: Double, y: Double, z: Double): Matrix4 = {
        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = 1
        matrix(1)(1) = 1
        matrix(2)(2) = 1
        matrix(3)(3) = 1
        matrix(3)(0) = x
        matrix(3)(1) = y
        matrix(3)(2) = z
        new Matrix4(matrix)
    }

    def rotationMatrix(rotation: Rotation) = xRotationMatrix(rotation.x) * yRotationMatrix(rotation.y) * zRotationMatrix(rotation.z)

    def xRotationMatrix(radian: Double) = {
        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = 1
        matrix(1)(1) = Math.cos(radian)
        matrix(1)(2) = -Math.sin(radian)
        matrix(2)(1) = Math.sin(radian)
        matrix(2)(2) = Math.cos(radian)
        matrix(3)(3) = 1
        new Matrix4(matrix)
    }

    def yRotationMatrix(radian: Double) = {
        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = Math.cos(radian)
        matrix(1)(1) = 1
        matrix(0)(2) = Math.sin(radian)
        matrix(2)(0) = -Math.sin(radian)
        matrix(2)(2) = Math.cos(radian)
        matrix(3)(3) = 1
        new Matrix4(matrix)
    }

    def zRotationMatrix(radian: Double) = {
        val matrix = Array.fill(4, 4)(0.0)
        matrix(0)(0) = Math.cos(radian)
        matrix(0)(1) = -Math.sin(radian)
        matrix(1)(0) = Math.sin(radian)
        matrix(1)(1) = Math.cos(radian)
        matrix(2)(2) = 1
        matrix(3)(3) = 1
        new Matrix4(matrix)
    }

    def pointAtMatrix(pos: Vector3, target: Vector3, up: Vector3) = {
        val newForward = (target - pos).normalized

        val temp = newForward * (up * newForward)
        val newUp = (up - temp).normalized

        val newRight = newUp.crossProduct(newForward)
        
        new Matrix4(Array(
            Array(newRight.x, newRight.y, newRight.z, 0.0),
            Array(newUp.x, newUp.y, newUp.z, 0.0),
            Array(newForward.x, newForward.y, newForward.z, 0.0),
            Array(pos.x, pos.y, pos.z, 1.0)
        ))
    }
}