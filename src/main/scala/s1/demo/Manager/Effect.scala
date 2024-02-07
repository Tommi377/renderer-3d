package s1.demo
import java.awt.image.BufferedImage

/**
 * This is the base class for every demo effect you implement.
 * 
 * @constructor Creates a new demo effect. Effects are shown in the [[DemoArea]].
 * @param width width of the effect in pixels
 * @param height height of the image in pixels
 */
abstract class Effect(val width: Int, val height: Int) {
  
  /**
   * Convenience method that creates an empty image of the effect's size
   * 
   * @return an empty image that is of the same size as the Effect itself.
   */
  def emptyImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    
  /**
   * Changes the state of the effect. Similar to the tick method in Flappy
   */
  def changeThings(): Unit
  
  /**
   * Creates a new java.awt.BufferedImage (or changes an existing one) that shows the current state of the effect. See the examples
   * for more implementation ideas.
   * 
   * @return the image that gets displayed in the [[DemoArea]]
   */
  
  def makePic(): BufferedImage
 
  /**
   * The effects can be manipulated with mouse if desired. This method is called whenever
   * mouse is hovered over the effect.
   * 
   * @param x the x-location of the mouse
   * @param y the y-location of the mouse
   */
  def mouseAt(x: Int, y: Int) = {}
  
  def keyPressed(key: String) = {}
  /**
   * This method tells the Demo engine when it is time to switch to the next effect
   * If this is the only effect, the method always returns '''false'''
   * 
   * @return true when this effect is over, false otherwise 
   */
  def next: Boolean
}