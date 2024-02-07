package s1.demo

import scala.swing.Component
import scala.collection.mutable.Buffer
import scala.swing.event._
import java.awt.Graphics2D
import java.util.{Timer, TimerTask}
import java.awt.BasicStroke
import java.awt.RenderingHints
import java.awt.Color._
import scala.collection.mutable.Queue
import java.awt.Font
import java.awt.image.BufferedImage
import s1.image.Color
import s1.image.ImageExtensions._
    
/**
 * DemoArea hosts one [[Effect]] at a time.
 * The code required to run it is already in place in the
 * [[Animate]] class.
 * 
 * THIS CLASS DOES NOT NEED TO BE MODIFIED
 * OR UNDERSTOOD
 */


class DemoArea(val effects: Buffer[Effect]) extends Component {
  demo =>
  
  var currentEffect = effects.head
  var nextEffects   = effects.tail
    
  var latest = emptyImage(currentEffect.height, currentEffect.width)    
  var mousePosition:Option[(Int, Int)] = None 
  
  override def paintComponent(g: Graphics2D) = {
    g.drawImage(latest, 0, 0, null)
  }

  def startAnimating(intervalMilliseconds: Int) = {
    val task = new TimerTask() {
      
      def run() = {
        currentEffect.changeThings()
        latest=currentEffect.makePic()
        mousePosition.foreach((pos) => currentEffect.mouseAt(pos._1, pos._2))

        if (currentEffect.next) {
          if (!nextEffects.isEmpty) {            
            currentEffect = nextEffects.head
            nextEffects  = nextEffects.tail
          }
        }
        
        demo.repaint()
      }
    }
    
    new Timer().schedule(task, 0, intervalMilliseconds)
  }
      
  reactions += {
    case mouse: MouseMoved => {
      currentEffect.mouseAt(mouse.point.getX.toInt, mouse.point.getY.toInt)
    }
    case KeyPressed(_, c, _, _) => currentEffect.keyPressed(c.toString)
  }
  
  listenTo(this.mouse.moves)
  listenTo(this.keys)
  focusable = true
}
