package jp.egen.yaguchi.game

trait Game {
  import scala.swing.{MainFrame,Component}
  import scala.swing.event.{InputEvent}
  import java.awt.{Dimension}

  val name : String
  val width : Int
  val height : Int
  val fps : Int

  var ui : UI = _

  val frame = new MainFrame {

    val glassPane = new Component {}
    peer.setGlassPane(glassPane.peer)
    peer.getGlassPane.setVisible(true)

    listenTo(glassPane.mouse.clicks)
    reactions += {
      case e : InputEvent => {
        if (ui!=null) ui.input(e)
      }
    }

    def createBufferStrategy(bufferCount:Int) = peer.createBufferStrategy(bufferCount)
    def getBufferStrategy = peer.getBufferStrategy

  }

  def init : Unit = {
    import actors.Actor.{actor,loop}

    frame.title = name
    frame.visible = true

    val insets = frame.peer.getInsets
    val w = width + insets.left + insets.right
    val h = height + insets.top + insets.bottom
    
    frame.size = new Dimension(w, h)

    frame.createBufferStrategy(2)
    val bufferStrategy = frame.getBufferStrategy

    actor {
      val startTime = System.currentTimeMillis
      var counter = 0
      loop {
        counter += 1

        val g = bufferStrategy.getDrawGraphics
        val insets = frame.peer.getInsets
        val gg = g.create(insets.left, insets.top, width, height)
        if (ui!=null) ui.update(gg)

        // show graphics
        bufferStrategy.show
        g.dispose

        // adjust FPS
        val currentTime = System.currentTimeMillis
        val supposedCurrentTime = startTime + counter * 1000 / fps
        val sleepingTime = supposedCurrentTime - currentTime
        if (sleepingTime>0) Thread.sleep(sleepingTime)

      }
    }
    
  }

}


abstract class UI {
  import scala.swing.event.{InputEvent}
  import java.awt.{Graphics}

  def update(g:Graphics) : Unit
  def input(ie:InputEvent) : Unit
}


abstract class Look {
  import java.awt.{Image,Graphics,Dimension}
  import java.awt.image.BufferedImage

  val size : Dimension
  def width : Int = size.width
  def height : Int = size.height

  def draw(g:Graphics, p:Point) : Unit = draw(g, p.x, p.y)
  def draw(g:Graphics) : Unit = draw(g, 0, 0)
  def draw(g:Graphics, x:Int, y:Int) : Unit

  def image : Image = {
    val imageType = BufferedImage.TYPE_4BYTE_ABGR
    val img = new BufferedImage(width, height, imageType)
    val g = img.getGraphics
    g.clearRect(0, 0, width, height)
    draw(g)
    img
  }
}


object Point {
  
  val origin = new Point(0, 0)
  val unit = new Point(1, 1)
  def apply(x:Int, y:Int) = new Point(x,y)
  /*implicit def awt2game(p:java.awt.Point) = new Point(p.x, p.y)*/
  /*implicit def game2awt(p:Point) = new java.awt.Point(p.x, p.y)*/
}

class Point(val x:Int, val y:Int) {
  import scala.math

  def +(that:Point) : Point = new Point(this.x+that.x, this.y+that.y)
  def -(that:Point) : Point  = new Point(this.x-that.x, this.y-that.y)
  def unary_- : Point = new Point(-this.x, -this.y)
  def *(that:Int) : Point  = new Point(this.x*that, this.y*that)
  def /(that:Int) : Point = new Point(this.x/that, this.y/that)
  def abs : Double= math.sqrt(this.absSq)
  def absSq :Int = this.x*this.x+this.y*this.y
  def distance(that:Point) : Double = (this-that).abs
  def distanceSq(that:Point) : Int = (this-that).absSq
}

