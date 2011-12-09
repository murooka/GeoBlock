package jp.egen.yaguchi.geoblock

import jp.egen.yaguchi.game.{UI,Point}
import scala.math._
import scala.util.{Random}
import scala.collection.mutable.{ArrayBuffer}
import scala.swing.event.{InputEvent,MousePressed,MouseReleased}
import java.awt.{Graphics,Color,Rectangle}

object Block {
  val None = 0
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
  val _blocks : Array[Array[Block]] = Array.ofDim[Block](width,height)
  for (w <- 0 until width; h <- 0 until height) {
    if (w>=2 || h>=2) {
      var flag = true
      while (flag) {
        val c = random.nextInt(colorCount) + 1
        val hflag = w<=2 || !(_blocks(w-1)(h).color==c && _blocks(w-2)(h).color==c)
        val vflag = h<=2 || !(_blocks(w)(h-1).color==c && _blocks(w)(h-2).color==c)
        if (hflag && vflag) {
          _blocks(w)(h) = new Block(c)
          flag = false
        }
      }
    } else {
      val c = random.nextInt(colorCount) + 1
      _blocks(w)(h) = new Block(c)
    }
  }

  def blocks(bx:Int, by:Int) : Block = _blocks(bx)(by)
  def blocks(bp:Point) : Block = _blocks(bp.x)(bp.y)
  def setBlock(bx:Int, by:Int, b:Block) = _blocks(bx)(by) = b
  def setBlock(bp:Point,b:Block) = _blocks(bp.x)(bp.y) = b



  def isDeadlock : Boolean = {
    false
  }


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
        val block = field.blocks(bx,by)
        if (block.fixed && block.color!=Block.None) paintFixedBlock(g, bx, by)
        if (focused==Point(bx,by)) paintSelectedBlock(g, bx, by)
      }
    }
  }

  def paintFixedBlock(g:Graphics, bx:Int, by:Int) = {
    assert(field.blocks(bx,by).color!=Block.None)
    val color = colors(field.blocks(bx,by).color)
    paintBlock(g, color, blockWidth*bx, blockHeight*by, blockWidth, blockHeight)
  }

  def paintSelectedBlock(g:Graphics, bx:Int, by:Int) = {
    assert(field.blocks(bx,by).color!=Block.None)
    val color = new Color(255,255,255,191)
    paintBlock(g, color, blockWidth*bx, blockHeight*by, blockWidth, blockHeight)
  }

  def paintBlock(g:Graphics, color:Color, x:Int, y:Int, w:Int, h:Int) : Unit = {
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
    field.blocks(src).fixed = false
    field.blocks(dst).fixed = false

    val vx = dst.x - src.x
    val vy = dst.y - src.y
  
    var counter = 0
    val countMax = 20

    def getLinedBlocks(pos:Point) : Array[Point] = {
      val color = field.blocks(pos).color

      def checkBlock(pos:Point, vec:Point) : ArrayBuffer[Point] = {
        val newPos = pos + vec
        if (newPos.x<0 || newPos.y<0 || newPos.x>=field.width || newPos.y>=field.height) {
          ArrayBuffer.empty[Point]
        } else if (field.blocks(newPos).color!=color) {
          ArrayBuffer.empty[Point]
        } else {
          checkBlock(newPos, vec) += newPos
        }
      }


      val varLine = (checkBlock(pos, Point(0,1)) ++ checkBlock(pos, Point(0,-1))).toArray
      val horLine = (checkBlock(pos, Point(1,0)) ++ checkBlock(pos, Point(-1,0))).toArray

      if (varLine.size>=2 && horLine.size>=2) {
        varLine ++ horLine ++ Array(pos)
      } else if (varLine.size>=2) {
        varLine ++ Array(pos)
      } else if (horLine.size>=2) {
        horLine ++ Array(pos)
      } else {
        Array.empty[Point]
      }
    }


    def isLined(pos:Point) = {
      val color = field.blocks(pos).color

      def countBlock(pos:Point, vec:Point) : Int = {
        val newPos = pos + vec
        if (newPos.x<0 || newPos.y<0 || newPos.x>=field.width || newPos.y>=field.height) {
          0
        } else if (field.blocks(newPos).color!=color) {
          0
        } else if (!field.blocks(newPos).fixed) {
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
        val temp = field.blocks(src)
        field.setBlock(src, field.blocks(dst))
        field.setBlock(dst, temp)
        
        val linedBlocks = (getLinedBlocks(src) ++ getLinedBlocks(dst)).distinct
        
        if (!rechange) {
          // fix blocks
          field.blocks(src).fixed = true
          field.blocks(dst).fixed = true
        } else if (!linedBlocks.isEmpty) {
          field.blocks(src).fixed = true
          field.blocks(dst).fixed = true
          field.events += new EraseEvent(linedBlocks)
        } else if (!linedBlocks.isEmpty) {
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
      assert(field.blocks(bx,by).color!=Block.None)
      val pos = (1.0 * counter / countMax)
      val x = bx * blockWidth + (pos * blockWidth * vx).toInt
      val y = by * blockHeight + (pos * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).color)
      paintBlock(g, color, x, y, w, h)
    }

    private def paintDst(g:Graphics) = {
      val bx = dst.x
      val by = dst.y
      assert(field.blocks(bx,by).color!=Block.None)
      val pos = - (1.0 * counter / countMax)
      val x = bx * blockWidth + (pos * blockWidth * vx).toInt
      val y = by * blockHeight + (pos * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).color)
      paintBlock(g, color, x, y, w, h)
    }
  
  }

  class EraseEvent(targets:Array[Point]) extends Event {

    for (t <- targets) {
      val block = new Block(Block.None)
      block.fixed = false
      field.setBlock(t, block)
    }
    val bottoms = targets.filter { (p:Point) =>
      val above = p - Point(0,1)
      if (above.y < 0) {
        false
      } else {
        field.blocks(above).color!=Block.None
      }
    }
    val fallings = bottoms.flatMap { (p:Point) =>
      val buffer = new ArrayBuffer[Point]
      var pos = p - Point(0,1)
      while (pos.y >= 0 && field.blocks(pos).color!=Block.None) {
        buffer += pos
        pos = pos - Point(0,1)
      }
      buffer.toList
    }.toArray
    for (p <- fallings) field.blocks(p).fixed = false

    var counter = 0

    def elapsed(time:Float) : Unit = {
      counter += 1
    }

    def ended : Boolean = {
      false
    }

    def paint(g:Graphics) : Unit = {
      for (p <- fallings) {
        paintFallingBlock(g, p.x, p.y)
      }
    }

    def paintFallingBlock(g:Graphics, bx:Int, by:Int) = {
      assert(field.blocks(bx,by).color!=Block.None)
      val x = bx * blockWidth
      val y = by * blockHeight + counter
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).color)
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
          val block = field.blocks(bx,by)
          if (block.fixed && block.color!=Block.None) focused = Point(bx, by)
        }
      }
      case MouseReleased(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
        if (fieldRect.contains(point) && focused!=null) {
          val bx = (point.x-fieldOffsetX) / blockWidth
          val by = (point.y-fieldOffsetY) / blockHeight
          val diffX = abs(bx-focused.x)
          val diffY = abs(by-focused.y)
          val block = field.blocks(bx,by)
          if (diffX+diffY==1 && block.fixed && block.color!=Block.None) {
            field.events += new ChangeEvent(focused, Point(bx, by))
          }
          focused = null
        }
      }
      case _ =>
    }
  }

}


