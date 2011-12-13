package jp.egen.yaguchi.geoblock

import jp.egen.yaguchi.game.{UI,Point,Look}
import scala.math._
import scala.util.{Random}
import scala.collection.mutable.{ListBuffer}
import scala.swing.event.{InputEvent,MousePressed,MouseReleased}
import java.awt.{Dimension,Graphics,Color,Rectangle}

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

class Block(var kind:Int) {
  var fixed : Boolean = true


  val look = new Look {

    val size = new Dimension(40,40)

    val color : Color = kind match {
      case Block.Red => Color.RED
      case Block.Green => Color.GREEN
      case Block.Blue => Color.BLUE
      case Block.Cyan => Color.CYAN
      case Block.Magenta => Color.MAGENTA
      case Block.Yellow => Color.YELLOW
      case Block.Orange => Color.ORANGE
      case _ => Color.BLACK
    }

    def draw(g:Graphics, x:Int, y:Int) : Unit = {
      g.clearRect(x, y, width, height)

      g.setColor(color)
      g.fillRect(      x+2,        y+2,  width-4,         2)
      g.fillRect(      x+2,        y+2,        2,  height-4)
      g.fillRect(      x+2, y+height-4,  width-4,         2)
      g.fillRect(x+width-4,        y+2,        2 , height-4)
      g.fillRect(      x+6,        y+6, width-12, height-12)
    }


  }

  override def toString = kind match {
    case Block.None => "None"
    case Block.Red => "Red"
    case Block.Green => "Green"
    case Block.Blue => "Blue"
    case Block.Cyan => "Cyan"
    case Block.Magenta => "Magenta"
    case Block.Yellow => "Yellow"
    case Block.Orange => "Orange"
    case _ => "Unknown"
  }

}

class GameField(val width:Int, val height:Int, val kindCount:Int) {
  val random = new Random
  val _blocks : Array[Array[Block]] = Array.ofDim[Block](width,height*2)
  for (w <- 0 until width; h <- 0 until height*2) {
    if (w>=2 || h>=2) {
      var flag = true
      while (flag) {
        val c = random.nextInt(kindCount) + 1
        val hflag = w<2 || !(_blocks(w-1)(h).kind==c && _blocks(w-2)(h).kind==c)
        val vflag = h<2 || !(_blocks(w)(h-1).kind==c && _blocks(w)(h-2).kind==c)
        if (hflag && vflag) {
          _blocks(w)(h) = new Block(c)
          flag = false
        }
      }
    } else {
      val c = random.nextInt(kindCount) + 1
      _blocks(w)(h) = new Block(c)
    }
  }

  def blocks(bx:Int, by:Int) : Block = _blocks(bx)(by)
  def blocks(bp:Point) : Block = _blocks(bp.x)(bp.y)
  def setBlock(bx:Int, by:Int, b:Block) = _blocks(bx)(by) = b
  def setBlock(bp:Point,b:Block) = _blocks(bp.x)(bp.y) = b

  def fallColumn(bp:Point) = {
    val column = _blocks(bp.x).toList
    val fallen = column.takeWhile(_.kind!=Block.None) ++ column.dropWhile(_.kind!=Block.None).dropWhile(_.kind==Block.None)
    val len = fallen.length
    val shortage = height * 2 - len
    val full = Array.tabulate[Block](shortage) { (_) => new Block(random.nextInt(kindCount) + 1) }.toList ++ fallen

    for (b <- full) b.fixed = true
    _blocks(bp.x) = full.toArray

  }



  val events = new ListBuffer[Event]
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
      for (by<- field.height until field.height*2) {
        val block = field.blocks(bx,by)
        if (block.fixed && block.kind!=Block.None) paintFixedBlock(g, bx, by)
        if (focused==Point(bx,by)) paintSelectedBlock(g, bx, by)
      }
    }
  }

  def paintFixedBlock(g:Graphics, bx:Int, by:Int) = {
    assert(field.blocks(bx,by).kind!=Block.None)
    val kind = colors(field.blocks(bx,by).kind)
    val pos = bpos2pos(Point(bx,by))
    paintBlock(g, kind, pos.x, pos.y, blockWidth, blockHeight)
  }

  def paintSelectedBlock(g:Graphics, bx:Int, by:Int) = {
    assert(field.blocks(bx,by).kind!=Block.None)
    val color = new Color(255,255,255,91)
    val pos = bpos2pos(Point(bx,by))
    paintBlock(g, color, pos.x, pos.y, blockWidth, blockHeight)
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


  def bpos2pos(bpos:Point) : Point = {
    val x = bpos.x * blockWidth
    val y = (bpos.y-field.height) * blockHeight
    Point(x,y)
  }

  def pos2bpos(pos:Point) : Point = {
    val fieldWidth = field.width * blockWidth
    val fieldHeight = field.height * blockHeight
    if (pos.x < 0 || fieldWidth <= pos.x || pos.y < 0 || fieldHeight <= pos.y) {
      null
    } else {
      val bx = pos.x / blockWidth
      val by = (pos.y + fieldHeight) / blockHeight
      Point(bx,by)
    }
  }


  class ChangeEvent(val src:Point, val dst:Point, rechange:Boolean=true) extends Event {
    field.blocks(src).fixed = false
    field.blocks(dst).fixed = false

    val vx = dst.x - src.x
    val vy = dst.y - src.y
  
    var counter = 0
    val countMax = 20

    def getLinedBlocks(pos:Point) : List[Point] = {
      val kind = field.blocks(pos).kind

      def checkBlock(pos:Point, vec:Point) : ListBuffer[Point] = {
        val newPos = pos + vec
        if (newPos.x<0 || field.width <= newPos.x || newPos.y < field.height || field.height*2 <= newPos.y) {
          ListBuffer.empty[Point]
        } else if (field.blocks(newPos).kind!=kind) {
          ListBuffer.empty[Point]
        } else {
          checkBlock(newPos, vec) += newPos
        }
      }


      val varLine = (checkBlock(pos, Point(0,1)) ++ checkBlock(pos, Point(0,-1))).toList
      val horLine = (checkBlock(pos, Point(1,0)) ++ checkBlock(pos, Point(-1,0))).toList

      if (varLine.size>=2 && horLine.size>=2) {
        varLine ++ horLine ++ List(pos)
      } else if (varLine.size>=2) {
        varLine ++ List(pos)
      } else if (horLine.size>=2) {
        horLine ++ List(pos)
      } else {
        List.empty[Point]
      }
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
      assert(field.blocks(bx,by).kind!=Block.None)
      val offset = (1.0 * counter / countMax)
      val pos = bpos2pos(Point(bx,by))
      val x = pos.x + (offset * blockWidth * vx).toInt
      val y = pos.y + (offset * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).kind)
      paintBlock(g, color, x, y, w, h)
    }

    private def paintDst(g:Graphics) = {
      val bx = dst.x
      val by = dst.y
      assert(field.blocks(bx,by).kind!=Block.None)
      val offset = - (1.0 * counter / countMax)
      val pos = bpos2pos(Point(bx,by))
      val x = pos.x + (offset * blockWidth * vx).toInt
      val y = pos.y + (offset * blockHeight * vy).toInt
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).kind)
      paintBlock(g, color, x, y, w, h)
    }
  
  }

  class EraseEvent(targets:List[Point]) extends Event {

    for (t <- targets) {
      val block = new Block(Block.None)
      block.fixed = false
      field.setBlock(t, block)
    }

    var bottoms = targets.filter { (p:Point) =>
      val above = p - Point(0,1)
      if (above.y < 0) {
        false
      } else {
        field.blocks(above).kind!=Block.None
      }
    }.map { (p:Point) =>
      p - Point(0,1)
    }

    for (bp <- fallingBlocks(bottoms)) field.blocks(bp).fixed = false

    def fallingBlocks(bottomList:List[Point]) : List[Point] = {
      bottomList.flatMap { (p:Point) =>
        val buffer = new ListBuffer[Point]
        var pos = p
        while (pos.y >= 0 && field.blocks(pos).kind!=Block.None) {
          buffer += pos
          pos = pos - Point(0,1)
        }
        buffer.toList
      }.toList
    }

    var counter = 0

    def offset = counter * 3

    def elapsed(time:Float) : Unit = {
      counter += 1
    }

    def ended : Boolean = {
      val landings = bottoms.filter { (bp:Point) =>
        val pos = bpos2pos(bp+Point(0,1))
        val fallpos = pos + Point(0, offset)
        val fallbpos = pos2bpos(fallpos)
        if (fallbpos==null || field.blocks(fallbpos).kind!=Block.None) true else false
      }
      for (p <- landings) field.fallColumn(p)
      bottoms = bottoms -- landings
      bottoms.isEmpty
    }

    def paint(g:Graphics) : Unit = {
      for (p <- fallingBlocks(bottoms)) {
        paintFallingBlock(g, p.x, p.y)
      }
    }

    def paintFallingBlock(g:Graphics, bx:Int, by:Int) = {
      assert(field.blocks(bx,by).kind!=Block.None)
      val pos = bpos2pos(Point(bx,by))
      val x = pos.x
      val y = pos.y + offset
      val w = blockWidth
      val h = blockHeight
      val color = colors(field.blocks(bx,by).kind)
      paintBlock(g, color, x, y, w, h)
    }

  }

  


  def input(ie:InputEvent) : Unit = {
    ie match {
      case MousePressed(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
        if (fieldRect.contains(point)) {
          val x = point.x - fieldOffsetX
          val y = point.y - fieldOffsetY
          println(x + ":" + y)
          val bpos = pos2bpos(Point(x,y))
          val block = field.blocks(bpos)
          if (block.fixed && block.kind!=Block.None) focused = bpos
        }
      }
      case MouseReleased(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(fieldOffsetX, fieldOffsetY, fieldWidth, fieldHeight)
        if (fieldRect.contains(point) && focused!=null) {
          val x = point.x - fieldOffsetX
          val y = point.y - fieldOffsetY
          val bpos = pos2bpos(Point(x,y))
          val diffX = abs(bpos.x-focused.x)
          val diffY = abs(bpos.y-focused.y)
          val block = field.blocks(bpos)
          if (diffX+diffY==1 && block.fixed && block.kind!=Block.None) {
            field.events += new ChangeEvent(focused, bpos)
          }
          focused = null
        }
      }
      case _ =>
    }
  }

}


