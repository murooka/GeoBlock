package jp.egen.yaguchi.geoblock

import jp.egen.yaguchi.game.{UI,Point,Look}
import scala.math._
import scala.util.{Random}
import scala.collection.mutable.{ListBuffer}
import scala.swing.event.{InputEvent,MousePressed,MouseReleased}
import java.awt.{Dimension,Graphics,Color,Rectangle}

object Block {
  val None = new Block(0)
  private val Red = new Block(1)
  private val Green = new Block(2)
  private val Blue = new Block(3)
  private val Cyan = new Block(4)
  private val Magenta = new Block(5)
  private val Yellow = new Block(6)
  private val Orange = new Block(7)

  val look = None.look

  private val r = new Random
  /**
   * return random Block expect "rejects"
   */
  def random(count:Int, rejects:List[Block] = List()) = {
    val blocks = List(Red,Green,Blue,Cyan,Magenta,Yellow,Orange).take(count).filterNot(rejects.contains)
    val index = r.nextInt(blocks.size)
    blocks(index).clone
  }

}

@cloneable
class Block(val kind:Int) {
  self =>

  var fixed : Boolean = true

  val look = new Look {

    val size = new Dimension(40,40)

    def color : Color = self match {
      case Block.Red     => Color.RED
      case Block.Green   => Color.GREEN
      case Block.Blue    => Color.BLUE
      case Block.Cyan    => Color.CYAN
      case Block.Magenta => Color.MAGENTA
      case Block.Yellow  => Color.YELLOW
      case Block.Orange  => Color.ORANGE
      case _ => Color.BLACK
    }

    def draw(g:Graphics, x:Int, y:Int) : Unit = {
      g.setColor(color)
      g.fillRect(      x+2,        y+2,  width-4,         2)
      g.fillRect(      x+2,        y+2,        2,  height-4)
      g.fillRect(      x+2, y+height-4,  width-4,         2)
      g.fillRect(x+width-4,        y+2,        2 , height-4)
      g.fillRect(      x+6,        y+6, width-12, height-12)
    }

  }

  override def equals(that:Any) = that match {
    case null => false
    case b:Block => this.kind==b.kind
    case _ => false
  }

  override def clone = super.clone.asInstanceOf[Block]

  override def toString = this match {
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

class GameField(val row:Int, val column:Int, val kindCount:Int) {
  val _blocks : Array[Array[Block]] = Array.ofDim[Block](row,column*2)
  for (w <- 0 until row; h <- 0 until column*2) {
    if (w>=2 || h>=2) {
      var flag = true
      while (flag) {
        val block = Block.random(kindCount)
        val hflag = w<2 || !(_blocks(w-1)(h)==block && _blocks(w-2)(h)==block)
        val vflag = h<2 || !(_blocks(w)(h-1)==block && _blocks(w)(h-2)==block)
        if (hflag && vflag) {
          _blocks(w)(h) = block
          flag = false
        }
      }
    } else {
      _blocks(w)(h) = Block.random(kindCount)
    }
  }

  def blocks(bx:Int, by:Int) : Block = _blocks(bx)(by)
  def blocks(bp:Point) : Block = _blocks(bp.x)(bp.y)
  def setBlock(bx:Int, by:Int, b:Block) = _blocks(bx)(by) = b
  def setBlock(bp:Point,b:Block) = _blocks(bp.x)(bp.y) = b

  def fallColumn(bp:Point) = {
    val line = _blocks(bp.x).toList
    val fallen = line.takeWhile(_!=Block.None) ++ line.dropWhile(_!=Block.None).dropWhile(_==Block.None)
    val len = fallen.length
    val shortage = column * 2 - len
    val full = Array.tabulate[Block](shortage) { (_) => Block.random(kindCount) }.toList ++ fallen

    for (b <- full) b.fixed = true
    _blocks(bp.x) = full.toArray

  }


  val look = new Look {

    val size = new Dimension(row * Block.look.width, column * Block.look.height)

    def bpos2pos(bpos:Point) : Point = {
      val width = Block.look.width
      val height = Block.look.height
      val x = bpos.x * width
      val y = (bpos.y-column) * height
      Point(x,y)
    }

    def pos2bpos(pos:Point) : Point = {
      val width = Block.look.width
      val height = Block.look.height
      val fieldWidth = row * width
      val fieldHeight = column * height
      if (pos.x < 0 || fieldWidth <= pos.x || pos.y < 0 || fieldHeight <= pos.y) {
        null
      } else {
        val bx = pos.x / width
        val by = (pos.y + fieldHeight) / height
        Point(bx,by)
      }
    }

    def draw(g:Graphics, x:Int, y:Int) : Unit = {
      for (bx <- 0 until row; by<- column until column*2) {
        val block = blocks(bx,by)
        if (block.fixed && block!=Block.None) {
          val bpos = Point(bx,by)
          val pos = bpos2pos(bpos)
          block.look.draw(g, x+pos.x, y+pos.y)
        }
      }
    }

  }


  val events = new ListBuffer[Event]

  def changeBlock(src:Point, dst:Point) = events += new ChangeEvent(src, dst)


  class ChangeEvent(val src:Point, val dst:Point, rechange:Boolean=true) extends Event {
    blocks(src).fixed = false
    blocks(dst).fixed = false

    val vx = dst.x - src.x
    val vy = dst.y - src.y
  
    var counter = 0
    val countMax = 20

    def getLinedBlocks(pos:Point) : List[Point] = {
      val kind = blocks(pos).kind

      def checkBlock(pos:Point, vec:Point) : ListBuffer[Point] = {
        val newPos = pos + vec
        if (newPos.x<0 || row <= newPos.x || newPos.y < column || column*2 <= newPos.y) {
          ListBuffer.empty[Point]
        } else if (blocks(newPos).kind!=kind) {
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
        val temp = blocks(src)
        setBlock(src, blocks(dst))
        setBlock(dst, temp)
        
        val linedBlocks = (getLinedBlocks(src) ++ getLinedBlocks(dst)).distinct
        
        if (!rechange) {
          // fix blocks
          blocks(src).fixed = true
          blocks(dst).fixed = true
        } else if (!linedBlocks.isEmpty) {
          blocks(src).fixed = true
          blocks(dst).fixed = true
          events += new EraseEvent(linedBlocks)
        } else if (!linedBlocks.isEmpty) {
        } else {
          events += new ChangeEvent(src, dst, false)
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
      val block = blocks(bx,by)
      assert(block!=Block.None)
      val offset = (1.0 * counter / countMax)
      val pos = look.bpos2pos(Point(bx,by))
      val w = block.look.width
      val h = block.look.height
      val x = pos.x + (offset * w * vx).toInt
      val y = pos.y + (offset * h * vy).toInt
      block.look.draw(g, x, y)
    }

    private def paintDst(g:Graphics) = {
      val bx = dst.x
      val by = dst.y
      val block = blocks(bx,by)
      assert(block!=Block.None)
      val offset = - (1.0 * counter / countMax)
      val pos = look.bpos2pos(Point(bx,by))
      val w = block.look.width
      val h = block.look.height
      val x = pos.x + (offset * w * vx).toInt
      val y = pos.y + (offset * h * vy).toInt
      block.look.draw(g, x, y)
    }
  
  }


  class EraseEvent(targets:List[Point]) extends Event {

    for (t <- targets) {
      val block = Block.None
      block.fixed = false
      setBlock(t, block)
    }

    var bottoms = targets.filter { (p:Point) =>
      val above = p - Point(0,1)
      if (above.y < 0) {
        false
      } else {
        blocks(above)!=Block.None
      }
    }.map { (p:Point) =>
      p - Point(0,1)
    }

    for (bp <- fallingBlocks(bottoms)) blocks(bp).fixed = false

    def fallingBlocks(bottomList:List[Point]) : List[Point] = {
      bottomList.flatMap { (p:Point) =>
        val buffer = new ListBuffer[Point]
        var pos = p
        while (pos.y >= 0 && blocks(pos)!=Block.None) {
          buffer += pos
          pos = pos - Point(0,1)
        }
        buffer.toList
      }.toList
    }

    var counter = 0

    def offset = counter * counter / 4

    def elapsed(time:Float) : Unit = {
      counter += 1
    }

    def ended : Boolean = {
      val landings = bottoms.filter { (bp:Point) =>
        val pos = look.bpos2pos(bp+Point(0,1))
        val fallpos = pos + Point(0, offset)
        val fallbpos = look.pos2bpos(fallpos)
        if (fallbpos==null || blocks(fallbpos)!=Block.None) true else false
      }
      for (p <- landings) fallColumn(p)
      bottoms = bottoms.filterNot(landings.contains)
      bottoms.isEmpty
    }

    def paint(g:Graphics) : Unit = {
      for (p <- fallingBlocks(bottoms)) {
        paintFallingBlock(g, p.x, p.y)
      }
    }

    def paintFallingBlock(g:Graphics, bx:Int, by:Int) = {
      val block = blocks(bx,by)
      assert(block!=Block.None)
      val pos = look.bpos2pos(Point(bx,by))
      val x = pos.x
      val y = pos.y + offset
      val w = block.look.width
      val h = block.look.height
      block.look.draw(g, x, y)
    }

  }

  




}


trait Event {
  def elapsed(time:Float) : Unit
  def ended : Boolean
  def paint(g:Graphics) : Unit
}


class GameUI(val field:GameField) extends GeoBlockUI {

  val fieldOffsetX = 0
  val fieldOffsetY = 100
  val fieldWidth = 320
  val fieldHeight = 320

  def bpos2pos(bpos:Point) = field.look.bpos2pos(bpos)
  def pos2bpos(pos:Point) = field.look.pos2bpos(pos)


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
    field.look.draw(fieldGraphics, 0, 0)

    for (e <- field.events) e.paint(fieldGraphics)

  }

  def paintFixedBlock(g:Graphics, bx:Int, by:Int) = {
    val block = field.blocks(bx,by)
    assert(block!=Block.None)
    val color = block.look.color
    val width = block.look.width
    val height = block.look.height
    val pos = bpos2pos(Point(bx,by))
    paintBlock(g, color, pos.x, pos.y, width, height)
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
          if (block.fixed && block!=Block.None) focused = bpos
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
          if (diffX+diffY==1 && block.fixed && block!=Block.None) {
            field.changeBlock(focused, bpos)
          }
          focused = null
        }
      }
      case _ =>
    }
  }

}
