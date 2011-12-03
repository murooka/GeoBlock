
package jp.egen.yaguchi.geoblock

import jp.egen.yaguchi.game._
import scala.swing.{MainFrame,Component}
import scala.actors.Actor._
import java.awt.{Font}

object GUIConfig {
  val width = 320
  val height = 480
  val fps = 60

  def font(size:Int) = new Font("Helvetica", Font.PLAIN, size)
}

import jp.egen.yaguchi.game

object GeoBlock extends Game {
  val name = "あっかりーん"
  val width = GUIConfig.width
  val height = GUIConfig.height
  val fps = 60

  ui = new TopUI

  def startGame = actor {
    val field = new GameField(8, 8, 5)
    GeoBlock.ui = new GameUI(field)
  }
  def showScore = println("score!")
  def showOption = println("option")

}

object Main {

  def main(args:Array[String]) : Unit = {
    val game = GeoBlock
    game.init
  }

}


import java.awt.{Rectangle,Graphics,Color}

abstract class GeoBlockUI extends UI {

  val width = GUIConfig.width
  val height = GUIConfig.height
  case class Menu(rect:Rectangle, name:String, event:()=>Unit) {

    def paint(g:Graphics) = {
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

}


class TopUI extends GeoBlockUI {

  val menuList = List(
    new Menu(new Rectangle( 20,220,130,40),   "Easy", GeoBlock.startGame _),
    new Menu(new Rectangle(170,220,130,40), "Normal", GeoBlock.startGame _),
    new Menu(new Rectangle( 20,280,130,40),   "Hard", GeoBlock.startGame _),
    new Menu(new Rectangle(170,280,130,40),    "One", GeoBlock.startGame _),
    new Menu(new Rectangle( 20,340,130,40),    "Two", GeoBlock.startGame _),
    new Menu(new Rectangle(170,340,130,40),   "Fire", GeoBlock.startGame _),
    new Menu(new Rectangle( 20,400,130,40),  "Score", GeoBlock.showScore _),
    new Menu(new Rectangle(170,400,130,40), "Option", GeoBlock.showOption _)
  )


  override def update(g:Graphics) = {
    g.setColor(Color.BLACK)
    g.fillRect(0,0,width,height)

    g.setFont(GUIConfig.font(84))
    g.setColor(Color.WHITE)
    g.drawString("Geo", 40, 100)
    g.drawString("Block", 70, 180)

    for (m <- menuList) m.paint(g)
  }


  import scala.swing.event._
  import scala.actors.Actor._
  import scala.swing.Reactions

  override def input(ie:InputEvent) : Unit = {
    ie match {
      case MousePressed(source, point, modifiers, clicks, triggersPopup) => {
        println("TopUI")
        menuList.find(_.rect.contains(point)).foreach(e => actor(e.event()))
      }

      case e => 
    }
  }

}


