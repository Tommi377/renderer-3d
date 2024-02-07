package s1.demo

import java.awt.image.BufferedImage
import java.awt.Color

//import s1.image.ImageExtensions._

object Gas extends Effect(500, 500){
  var time = 0
  var amplitud = 0.0
  val step = 0.01
  var increasing = true
  
  val current = this.emptyImage
  
  def makePic() = current
  
  private def getPixel(x: Int, y: Int) = {
    val c = math.abs(
        (
            (x + time) / 100.0 + 
            math.sin((y + (time / 40.0)) / (time / 50.0 % 20 + 30)) / 2+ 
            math.sin((y + time / 2.0) / 10.0) / 4 + 
            math.sin((y + time) / 3.0) / 10 + 
            math.sin(y / 5.0) * this.amplitud
            //math.sin((x % (50)) / 5.0) / 10
        ) * 255
    ).toInt % 255
    new Color(0, c, c / 4).getRGB
  }
  
  def changeThings() = {
    var x = 0
    var y = 0
    val w = this.current.getWidth
    var h = this.current.getHeight
    while(y < h){
      val c = this.getPixel(x, y)
      this.current.setRGB(x, y, c)
      x += 1
      if(x == w){
        x = 0
        y += 1
      }
    }
    
    this.time += 1
    if(this.amplitud <= -0.3){
      this.increasing = true
      this.amplitud += this.step
    }else if(this.amplitud >= 0.3){
      this.increasing = false
      this.amplitud -= this.step
    }else if(this.increasing){
      this.amplitud += this.step
    }else{
      this.amplitud -= this.step
    }
  }
  
  def next = false
}