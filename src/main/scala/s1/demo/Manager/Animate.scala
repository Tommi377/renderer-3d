package s1.demo

import scala.swing.SimpleSwingApplication
import scala.swing.Swing._ // This adds a few convenient shortcuts
import scala.swing.MainFrame
import scala.collection.mutable.Buffer


/**
 * Animate inherits SimpleSwingApplication which makes
 * this a graphical user interface
 */
object Animate extends SimpleSwingApplication {

  // In this list you can blace all the effects you have
  // Effects are changed when the current effect returns true from it's
  // "next"-method
  val area = new DemoArea(Buffer(Demo, TitleAnimation, TrippyTeapot, OffsetStuff, Gas))

  val top = new MainFrame() {
    preferredSize = (500, 500)
    contents = area
  }

  top.visible = true

  // This sets the delay between frames
  area.startAnimating(50)
}
