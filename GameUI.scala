package jp.egen.yaguchi.geoblock

import jp.egen.yaguchi.game.{UI,Point}
import scala.math._
import scala.util.{Random}
import scala.collection.mutable.{ArrayBuffer}
import scala.swing.event.{InputEvent,MousePressed,MouseReleased}
import java.awt.{Graphics,Color,Rectangle}

object Block {
  val Red = 1
  val Green = 2
  val Blue = 3
  val Cyan = 4
  val Magenta = 5
  val Yellow = 6
  val Orange = 7
}

class Block(val color:Int) {
  var fixed : Boolean = true
}

class GameField(val width:Int, val height:Int, val colorCount:Int) {
  val random = new Random
  val blocks = Array.tabulate[Block](width,height) ( (w,h) => {
    val c = random.nextInt(colorCount) + 1
    new Block(c)
  })
  def blockAt(p:Point) = blocks(p.x)(p.y)


  val events = new ArrayBuffer[Event]
}


trait Event {
  def elapsed(time:Float) : Unit
  def ended : Boolean
  def paint(g:Graphics) : Unit
}


class GameUI(val field:GameField) extends GeoBlockUI {

  val colors = Array(
    Color.WHITE,
    Color.RED,
    Color.GREEN,
    Color.BLUE,
    Color.CYAN,
    Color.MAGENTA,
    Color.YELLOW,
    Color.ORANGE
  )

  val fieldOffsetX = 0
  val fieldOffsetY = 100
  val fieldWidth = 320
  val fieldHeight = 320

  val blockWidth = fieldWidth / field.width
  val blockHeight = fieldHeight / field.height


  var focused : Point = _

  def update(g:Graphics) : Unit = {

    // update data
    var events = field.events
    var o = events.headOption
    while (!o.isEmpty) {
      val e = o.get
      if (e.ended) {
        field.events -= e
      } else {
        e.elapsed(16)
      }
      
      if (!events.isEmpty) events = events.tail
      o = events.headOption
    }


    // update graphics
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)

    val fieldGraphics = g.create(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
    paintField(fieldGraphics)

    for (e <- field.events) e.paint(fieldGraphics)

  }

  def paintField(g:Graphics) = {
    for (bx <- 0 until field.width) {
      for (by<- 0 until field.height) {
        if (field.blocks(bx)(by).fixed) paintFixedBlock(g, bx, by)
        if (focused==Point(bx,by)) paintSelectedBlock(g, bx, by)
      }
    }
  }

  def paintFixedBlock(g:Graphics, bx:Int, by:Int) = {
    val color = colors(field.blocks(bx)(by).color)
    paintBlock(g, color, blockWidth*bx, blockHeight*by, blockWidth, blockHeight)
  }

  def paintSelectedBlock(g:Graphics, bx:Int, by:Int) = {
    val color = new Color(255,255,255,191)
    paintBlock(g, color, blockWidth*bx, blockHeight*by, blockWidth, blockHeight)
  }

  def paintBlock(g:Graphics, color:Color, x:Int, y:Int, w:Int, h:Int) = {
    g.setColor(Color.BLACK)
    g.fillRect(x+2, y+2, w-4, h-4)
    g.setColor(color)
    g.fillRect(x+2,   y+2, w-4, 2)
    g.fillRect(x+2,   y+2, 2, h-4)
    g.fillRect(x+2, y+h-4, w-4, 2)
    g.fillRect(x+w-4, y+2, 2, h-4)
    g.fillRect(x+6, y+6, w-12, h-12)
  }



  class ChangeEvent(val src:Point, val dst:Point, rechange:Boolean=true) extends Event {
    field.blocks(src.x)(src.y).fixed = false
    field.blocks(dst.x)(dst.y).fixed = false

    val vx = dst.x - src.x
    val vy = dst.y - src.y
  
    var counter = 0
    val countMax = 20

    def getLinedBlocks(pos:Point) = {
      val color = field.blocks(pos.x)(pos.y).color

      def checkBlock(pos:Point, vec:Point) : Array[Point] = {
        val newPos = pos + vec
        if (field.blocks(newPos.x)(newPos.y).color!=color) {
          Array()
        } else {
          Array(newPos) ++ checkBlock(newPos, vec)
        }
      }


      Array(pos) ++ checkBlock(pos, Point(0,1)) ++ checkBlock(pos, Point(0,-1)) ++ 
                        checkBlock(pos, Point(1,0)) ++ checkBlock(pos, Point(-1,0))
    }


    def isLined(pos:Point) = {
      val color = field.blocks(pos.x)(pos.y).color

      def countBlock(pos:Point, vec:Point) : Int = {
        val newPos = pos + vec
        if (newPos.x<0 || newPos.y<0 || newPos.x>=field.width || newPos.y>=field.height) {
          0
        } else if (field.blockAt(newPos).color!=color) {
          0
        } else if (!field.blockAt(newPos).fixed) {
          0
        } else {
          1 + countBlock(newPos, vec)
        }
      }

      val vertical = 1 + countBlock(pos, Point(0,1)) + countBlock(pos, Point(0,-1))
      val horizontal = 1 + countBlock(pos, Point(1,0)) + countBlock(pos, Point(-1,0))
      
      vertical >= 3 || horizontal >= 3
    }

    def elapsed(time:Float) = {
      counter += 1
    }
  
    def ended : Boolean = {
      if (counter>=countMax) {
        // change blocks
        val temp = field.blockAt(src)
        field.blocks(src.x)(src.y) = field.blockAt(dst)
        field.blocks(dst.x)(dst.y) = temp
        
        
        if (isLined(src) || isLined(dst) || !rechange) {
          // fix blocks
          field.blockAt(src).fixed = true
          field.blockAt(dst).fixed = true
        } else {
          field.events += new ChangeEvent(src, dst, false)
        }


        true
      } else {
        false
      }
    }
  
    def paint(g:Graphics) : Unit = {
      paintDst(g)
      paintSrc(g)
    }

    private def paintSrc(g:Graphics) = {
      val bx = src.x
      val by = src.y
      val pos = (1.0 * counter / countMax)
      val x = bx * blockWidth + (pos * blockWidth * vx).toInt
      val y = by * blockHeight + (pos * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx)(by).color)
      paintBlock(g, color, x, y, w, h)
    }

    private def paintDst(g:Graphics) = {
      val bx = dst.x
      val by = dst.y
      val pos = - (1.0 * counter / countMax)
      val x = bx * blockWidth + (pos * blockWidth * vx).toInt
      val y = by * blockHeight + (pos * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx)(by).color)
      paintBlock(g, color, x, y, w, h)
    }
  
  }



  def input(ie:InputEvent) : Unit = {
    ie match {
      case MousePressed(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
        if (fieldRect.contains(point)) {
          val bx = (point.x-fieldOffsetX) / blockWidth
          val by = (point.y-fieldOffsetY) / blockHeight
          if (field.blocks(bx)(by).fixed) focused = Point(bx, by)
        }
      }
      case MouseReleased(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
        if (fieldRect.contains(point) && focused!=null) {
          val bx = (point.x-fieldOffsetX) / blockWidth
          val by = (point.y-fieldOffsetY) / blockHeight
          val diffX = abs(bx-focused.x)
          val diffY = abs(by-focused.y)
          if (diffX+diffY==1 && field.blocks(bx)(by).fixed) {
            field.events += new ChangeEvent(focused, Point(bx, by))
          }
          focused = null
        }
      }
      case _ =>
    }
  }

}


