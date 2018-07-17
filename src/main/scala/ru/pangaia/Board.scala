package ru.pangaia

import java.awt.{Color, Graphics, Graphics2D, Toolkit}
import java.awt.event.{ActionEvent, ActionListener, KeyAdapter, KeyEvent}
import javax.swing.{ImageIcon, JPanel}

import scala.util.Sorting

/**
  * Created by oneuro on 06.02.17.
  */
class Board extends JPanel with ActionListener
{
  val stage = new Stage(Config.maxS,Config.maxZ)
  val timer = new AcceleratingTimer(this)


  def init(): Unit =
  {
    setBackground(Color.BLACK)
    setFocusable(true)
    addKeyListener(new KAdapter())
    stage.init
    stage.addRoom(2, 6, 8, 6)
    stage.addRoom(12, 14, 4, 4)
    stage.addSimpleSnake((2,(Config.maxZ-1)/2),(1,(Config.maxZ-1)/2),(0,(Config.maxZ-1)/2))
    stage.hexes((19,11)).addContent(GreenApple(100))
    stage.hexes((21,11)).addContent(SpiderCrossed1("olya"))
  }
  init()
  repaint()
  timer.start()

  class KAdapter extends KeyAdapter
  {
    override def keyPressed(keyEvent: KeyEvent): Unit =
    {
      val code = keyEvent.getKeyCode
      code match
      {
        case KeyEvent.VK_NUMPAD6 => stage.getSnakeHead.direction = Utils.RIGHT
        case KeyEvent.VK_NUMPAD3 => stage.getSnakeHead.direction = Utils.RIGHT_LOW
        case KeyEvent.VK_NUMPAD9 => stage.getSnakeHead.direction = Utils.RIGHT_HI
        case KeyEvent.VK_NUMPAD1 => stage.getSnakeHead.direction = Utils.LEFT_LOW
        case KeyEvent.VK_NUMPAD7 => stage.getSnakeHead.direction = Utils.LEFT_HI
        case KeyEvent.VK_NUMPAD4 => stage.getSnakeHead.direction = Utils.LEFT
        case _ => ()
      }
    }
  }

  override def paintComponent(graphics: Graphics): Unit =
  {
    super.paintComponent(graphics)
    doDrawing(graphics)
    Toolkit.getDefaultToolkit.sync()
  }

  private def doDrawing(g: Graphics): Unit =
  {
    val g2d : Graphics2D = g.asInstanceOf[Graphics2D]
    for ((x,y) <- stage.hexes.keys)
    {
      val hexContents = stage.hexes((x,y)).contents.toArray
      Sorting.quickSort(hexContents)(Ordering[Int].on(p=>p.zIndex))

      for (cont <- hexContents)
      {
        val img = new ImageIcon(cont.spriteFile)
        val viewCoords = Utils.getScreenXY(x,y,img.getIconWidth,img.getIconHeight)
        g2d.drawImage(img.getImage,viewCoords._1,viewCoords._2,this)
      }

    }
  }

  private def nextLevel: Boolean =
  {
    this.stage.level.nextLevel
  }

  override def actionPerformed(actionEvent: ActionEvent): Unit =
  {
    val lev = this.stage.level

    lev.takeTurn()
    stage.moveSnake()
//    if (nextLevel)
//    {
//      lev.level0+=1
//      println("level: " + lev.level0)
//      timer.restartWithDelay(lev.getDelayForLevel(lev.level0))
//    }
    repaint()

  }
}
