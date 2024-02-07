package s1.image

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints

package object ImageExtensions {
  
   /**
    * This allows easier creation of BufferedImages)
    *
     {{{
        val example = emptyImage(100,100)
     }}} 
     @param width the width of the image
     @param height the height of the image
     @return a new, completely transparent image (All pixels are initially Color(0,0,0,0))
     */

  def emptyImage(width: Int, height: Int) = {
    new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  }
  
  implicit class S1Image(b: BufferedImage) {
    
    /**
     * This allows easier copying of a BufferedImage
     *
     {{{
        val example = emptyImage(100,100)
        
        val copy = example.copy
     }}} 
     
     * return a copy of this image
     */
    def copy = {
      val copied = emptyImage(b.getWidth, b.getHeight)
      copied.getGraphics.drawImage(b, 0, 0, null)
      copied
    }
    
    /**
     * This allows easier access to the Graphics2D object used to draw on painting surfaces (e.g. BufferedImages)
     *
     {{{
        val example = emptyImage(100,100)
        
        val g = example.graphics
     }}} 
     * @return the graphics object used when drawing on images
     */
    def graphics = {
      val g = b.getGraphics.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g
    }
    
    /**
     * These three methods allow accessing individual pixels in a BufferedImage in a more
     * convenient way
      
      {{{
        val example = emptyImage(100,100)
        
        // Set Color at x=10, y=5 
        example(10, 5).color = Color(50,100,150)
     
        // Get color at x=10, y=5
        val pixel = example(10, 5).color
        
        // Is a pixel opaque?
        val isItOpaque = example(10, 5).opaque 
       }}}
     */
    
    
    @inline
    def apply(x: Int, y: Int) = new {
      @inline
      def opaque = (b.getRGB(x, y) & 0xFF000000) == 0xFF000000
      @inline
      def color_=(c: Color) = b.setRGB(x, y, c.argb)
      @inline
      def color = Color(b.getRGB(x, y))
    }
  }
}