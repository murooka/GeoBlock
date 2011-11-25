import scala.swing._
import java.awt.{Graphics2D,Color,Font,Dimension,Rectangle}

package jp.egen.yaguchi.geoblock {

  object Main {

    def main(args:Array[String]) = {
      frame.pack
      frame.visible = true
    }

    val frame = new MainFrame {
      var ui_ : UserInterface = new TopUI
      def ui_=(newui:UserInterface) = {
        contents.head.reactions -= ui_.input
        contents.head.reactions += newui.input
        ui_ = newui
      }
      def ui = ui_

      title = "GeoBlock"
      resizable = false
      contents = new Panel {

        val width = GUIConfig.width
        val height = GUIConfig.height
        
        preferredSize = new Dimension(width,height)

        override protected def paintComponent(g:Graphics2D) = ui.output(g)
        listenTo(this.mouse.clicks)
        reactions += ui.input

      }

    }

  }


  object GUIConfig {
    val width = 320
    val height = 480

    def font(size:Int) = new Font("Helvetica", Font.PLAIN, size)
  }


  import scala.swing.Reactions

  abstract class UserInterface {
    case class Menu(rect:Rectangle, name:String, event:()=>Unit) {

      def paint(g:Graphics2D) = {
        val x = rect.x
        val y = rect.y
        val w = rect.width
        val h = rect.height

        g.setColor(Color.DARK_GRAY)
        g.fillRect(x-1,y-1,w+2,h+2)
        g.setColor(Color.WHITE)
        g.fillRect(x,y,w,h)
        g.setColor(Color.LIGHT_GRAY)
        g.fillRect(x+1,y+1,w-2,h-2)
        g.setColor(Color.GRAY)
        g.fillRect(x+2,y+2,w-4,h-4)
        g.setColor(Color.DARK_GRAY)
        g.fillRect(x+3,y+3,w-6,h-6)

        g.setFont(GUIConfig.font(30))
        g.setColor(Color.WHITE)
        var fontMetrics = g.getFontMetrics
        var strWidth = fontMetrics.stringWidth(name)
        g.drawString(name,x+(w-strWidth)/2, y+30)

      }

    }

    def output(g:Graphics2D) : Unit
    val input : Reactions.Reaction

  }


  class TopUI extends UserInterface {

    val menuList = List(
      new Menu(new Rectangle( 20,220,130,40),   "Easy", GameManager.startGame _),
      new Menu(new Rectangle(170,220,130,40), "Normal", GameManager.startGame _),
      new Menu(new Rectangle( 20,280,130,40),   "Hard", GameManager.startGame _),
      new Menu(new Rectangle(170,280,130,40),    "One", GameManager.startGame _),
      new Menu(new Rectangle( 20,340,130,40),    "Two", GameManager.startGame _),
      new Menu(new Rectangle(170,340,130,40),   "Fire", GameManager.startGame _),
      new Menu(new Rectangle( 20,400,130,40),  "Score", GameManager.showScore _),
      new Menu(new Rectangle(170,400,130,40), "Option", GameManager.showOption _)
    )

      val width = GUIConfig.width
      val height = GUIConfig.height

    override def output(g:Graphics2D) = {
      g.setBackground(Color.BLACK)
      g.clearRect(0,0,width,height)

      g.setFont(GUIConfig.font(84))
      g.setColor(Color.WHITE)
      g.drawString("Geo", 40, 100)
      g.drawString("Block", 70, 180)

      for (m <- menuList) m.paint(g)
    }


    import scala.swing.event._
    import scala.actors.Actor._
    import scala.swing.Reactions

    override val input : Reactions.Reaction = {
      case MousePressed(source, point, modifiers, clicks, triggersPopup) => {
        println("TopUI")
        menuList.find(_.rect.contains(point)).foreach(e => actor(e.event()))
      }
    }

  }


  class GameUI(field:GameField) extends UserInterface {

    import scala.swing.event._
    val width = GUIConfig.width
    val height = GUIConfig.height

    val menu = new Menu(new Rectangle(5,20,100,40), "Menu", {() => ()})

    val colors = Array(
      Color.BLACK,
      Color.RED,
      Color.GREEN,
      Color.BLUE,
      Color.CYAN,
      Color.MAGENTA,
      Color.YELLOW,
      Color.ORANGE,
      new Color(255,255,255, 64)
    )

    override def output(g:Graphics2D) : Unit = {
      g.setBackground(Color.BLACK)
      g.clearRect(0,0,width,height)

      val fieldGrpahics = g.create(0, 100, 320, 320)
      paintField(fieldGrpahics)
      
    }

    import java.awt.Graphics

    def paintField(g:Graphics) = {
      val r = g.getClipBounds
      val fieldWidth = r.width
      val fieldHeight = r.height
      
      val blockWidth = fieldWidth / field.x
      val blockHeight = fieldHeight / field.y

      for (bx <- 0 to field.x-1; by <- 0 to field.y-1 if field.blocks(bx)(by).fixed) {
        val x = bx * blockWidth
        val y = by * blockHeight
        val cellGraphics = g.create(x, y, blockWidth, blockHeight)
        paintBlock(cellGraphics, field.blocks(bx)(by).kind)
      }

      val focusedBPos = field.focusedBPos
      if (focusedBPos != null) {
        val bx = focusedBPos.x
        val by = focusedBPos.y
        val x = bx * blockWidth
        val y = by * blockHeight
        val cellGraphics = g.create(x, y, blockWidth, blockHeight)
        paintBlock(cellGraphics, 8)
      }

    }

    def paintBlock(g:Graphics, kind:Int) = {
      val r = g.getClipBounds
      val w = r.width
      val h = r.height

      g.setColor(colors(kind)); g.fillRect(1, 1, w-2, h-2)
      g.setColor(new Color(0, 0, 0, 64)); g.fillRect(4, 4, w-8, h-8)
      g.setColor(new Color(0, 0, 0, 64)); g.fillRect(5, 5, w-10, h-10)
      g.setColor(new Color(0, 0, 0, 64)); g.fillRect(6, 6, w-12, h-12)
      g.setColor(new Color(0, 0, 0, 64)); g.fillRect(7, 7, w-14, h-14)
      g.setColor(colors(kind)); g.fillRect(8, 8, w-16, h-16)
    }

    override val input : Reactions.Reaction = {
      case MousePressed(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(0, 100, 320, 320)
        val blockWidth = 320 / field.x
        val blockHeight = 320 / field.y
        if (fieldRect.contains(point)) {
          val mx = point.x
          val my = point.y - 100
          val bx = mx / blockWidth
          val by = my / blockHeight
          println("clicked " + bx + "," + by)
          field.focusBlock(bx, by)
          Main.frame.repaint
        }
      }


      case MouseReleased(source, point, modifiers, clicks, triggersPopup) => {
        val fieldRect = new Rectangle(0, 100, 320, 320)
        val blockWidth = 320 / field.x
        val blockHeight = 320 / field.y
        if (fieldRect.contains(point)) {
          val mx = point.x
          val my = point.y - 100
          val bx = mx / blockWidth
          val by = my / blockHeight
          println("released " + bx + "," + by)
          field.changeBlock(bx, by)
          field.removeFocus
        }
      }
    }
 
  }

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

  case class Block(kind:Int) {
    var fixed : Boolean = true
  }

  class GameField(val x:Int, val y:Int, colorCount:Int) {
    import scala.util.Random
  
    val random = new Random
    val _blocks = Array.tabulate(x, y*2)((bx,by) => {
      val kind = random.nextInt(colorCount) + 1
      new Block(kind)
    })

    def blocks = _blocks
    def blocks(bpos:Point) = _blocks(bpos.x)(bpos.y)

    var focusedBPos : Point = null
    def focusBlock(bx:Int, by:Int) = focusedBPos = new Point(bx,by)
    def removeFocus = focusedBPos = null

    def changeBlock(bx:Int, by:Int) : Unit = {
      if (focusedBPos==null) return;
      val diff = math.abs(focusedBPos.x - bx) + math.abs(focusedBPos.y - by)
      if (diff==1) {
        blocks(focusedBPos).fixed = false
        blocks(bx)(by).fixed = false

        import scala.actors.Actor._

        Main.frame.repaint
      }
    }

  }

  object GameManager {

    import scala.actors.Actor._

    def startGame = {
      actor {
        val field = new GameField(8, 8, 5)
        Main.frame.ui = new GameUI(field)
        Main.frame.repaint
      }
    }

    def showScore = {
      println("score!")
    }

    def showOption = {
      println("option")
    }

  }


}
