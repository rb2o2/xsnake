package ru.pangaia

import java.awt.{Color, Graphics, Graphics2D, Toolkit}
import java.awt.event.{ActionEvent, ActionListener, KeyAdapter, KeyEvent}
import javax.swing.{ImageIcon, JPanel}
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Sorting
import scala.util.Random

/**
  * Created by oneuro on 06.02.17.
  */
class Board extends JPanel with ActionListener :
  val stage = new Stage(Config.maxS, Config.maxZ)
  val timer = new AcceleratingTimer(this)
  val gameoverHexes: mutable.Set[(Int, Int)] = {
    val ms = Config.maxS; val mz = Config.maxZ
    mutable.Set(
      (ms/2, mz/2),
      (ms/2, mz/2-1),
      (ms/2, mz/2-2),
      (ms/2, mz/2-3),
      (ms/2, mz/2+1),
      (ms/2, mz/2+2),
      (ms/2, mz/2+3),

      (ms/2+1, mz/2),
      (ms/2+2, mz/2),
      (ms/2+3, mz/2),

      (ms/2+3, mz/2),
      (ms/2+4, mz/2-1),
      (ms/2+3, mz/2-2),
      (ms/2+4, mz/2-3),
      (ms/2+4, mz/2+1),
      (ms/2+3, mz/2+2),
      (ms/2+4, mz/2+3)

    ).map((a1,a2) => (a1 -2, a2+1)) ++ mutable.Set(
      (ms/2, mz/2-3),
      (ms/2, mz/2-2),
      (ms/2, mz/2-1),
      (ms/2, mz/2),
      (ms/2, mz/2+1),
      (ms/2, mz/2+2),
      (ms/2, mz/2+3),

      (ms/2+1, mz/2-3),
      (ms/2+2, mz/2-3),
      (ms/2+3, mz/2-3),
      (ms/2+1, mz/2+3),
      (ms/2+2, mz/2+3),
      (ms/2+3, mz/2+3),
      (ms/2+1, mz/2),
      (ms/2+2, mz/2)

    ).map((a1,a2)=> (a1 + 4, a2 + 1)) ++ mutable.Set(
      (ms/2, mz/2-3),
      (ms/2, mz/2-2),
      (ms/2, mz/2-1),
      (ms/2, mz/2),
      (ms/2, mz/2+1),
      (ms/2, mz/2+2),
      (ms/2, mz/2+3),

      (ms/2+4, mz/2-3),
      (ms/2+3, mz/2-2),
      (ms/2+4, mz/2-1),
      (ms/2+3, mz/2),
      (ms/2+4, mz/2+1),
      (ms/2+3, mz/2+2),
      (ms/2+4, mz/2+3),

      (ms/2+1, mz/2+3),
      (ms/2+2, mz/2+3),
      (ms/2+3, mz/2+3),

      (ms/2+3, mz/2+5),

      (ms/2+3, mz/2+4)

    ).map((a1,a2) => (a1 + 9, a2 + 1)) ++ mutable.Set(
      (ms/2, mz/2),
      (ms/2, mz/2-1),
      (ms/2, mz/2-2),
      (ms/2+1, mz/2-3),
      (ms/2, mz/2+1),
      (ms/2, mz/2+2),
      (ms/2+1, mz/2+3),

      (ms/2+2, mz/2+3),
      (ms/2+2, mz/2-3),

      (ms/2+3, mz/2),
      (ms/2+4, mz/2-1),
      (ms/2+3, mz/2-2),
      (ms/2+3, mz/2-3),
      (ms/2+4, mz/2+1),
      (ms/2+3, mz/2+2),
      (ms/2+3, mz/2+3)

    ).map((a1,a2) => (a1 - 8, a2 + 1)) ++ mutable.Set(
      (ms/2, mz/2),
      (ms/2, mz/2-1),
      (ms/2, mz/2-2),
      (ms/2, mz/2-3),
      (ms/2, mz/2+1),
      (ms/2, mz/2+2),
      (ms/2, mz/2+3),

      (ms/2+1, mz/2),
      (ms/2+2, mz/2),

      (ms/2+3, mz/2-1),
      (ms/2+3, mz/2-2),
      (ms/2+4, mz/2-3),
      (ms/2+3, mz/2+1),
      (ms/2+3, mz/2+2),
      (ms/2+4, mz/2+3)
    ).map((a1,a2) => (a1 - 14, a2 + 1))
  }
  val hexesClearedForGameOver: collection.mutable.Set[(Int, Int)] = collection.mutable.Set()


  def init(): Unit =
    setBackground(Color.BLACK)
    setFocusable(true)
    addKeyListener(new KAdapter())
    stage.init()
    stage.addRoom(2, 6, 8, 6)
    stage.addRoom(12, 14, 4, 4)
    stage.addSimpleSnake((2,(Config.maxZ-1)/2),(1,(Config.maxZ-1)/2),(0,(Config.maxZ-1)/2))
    stage.hexes((19,11)).addContent(GreenApple(100))
    val spider = SpiderCrossed1("olya")
    stage.hexes((21,11)).addContent(spider)
    stage.enemies += "olya" -> spider
    stage.hexes((21,11)).addContent(SpiderSilkWeb())
    stage.hexes((22,11)).addContent(SpiderSilkWeb())
    stage.hexes((23,11)).addContent(SpiderSilkWeb())
    stage.hexes((25,15)).addContent(SpiderSilkWeb())
    stage.hexes((24,15)).addContent(SpiderSilkWeb())
    stage.hexes((25,17)).addContent(SpiderSilkWeb())

  init()
  repaint()
  timer.start()

  class KAdapter extends KeyAdapter :

    override def keyPressed(keyEvent: KeyEvent): Unit =
      val code = keyEvent.getKeyCode
      code match
        case KeyEvent.VK_NUMPAD6 => stage.getSnakeHead.direction = Utils.RIGHT
        case KeyEvent.VK_NUMPAD3 => stage.getSnakeHead.direction = Utils.RIGHT_LOW
        case KeyEvent.VK_NUMPAD9 => stage.getSnakeHead.direction = Utils.RIGHT_HI
        case KeyEvent.VK_NUMPAD1 => stage.getSnakeHead.direction = Utils.LEFT_LOW
        case KeyEvent.VK_NUMPAD7 => stage.getSnakeHead.direction = Utils.LEFT_HI
        case KeyEvent.VK_NUMPAD4 => stage.getSnakeHead.direction = Utils.LEFT
        case KeyEvent.VK_G => stage.endgame(); timer.restartWithDelay(100)
        case _ => ()

  def endgame(): Unit =
    if (!stage.gameOver) {
      stage.gameOver = true
      timer.restartWithDelay(100)
    }

  override def paintComponent(graphics: Graphics): Unit =
    super.paintComponent(graphics)
    doDrawing(graphics)
    Toolkit.getDefaultToolkit.sync()

  private def doDrawing(g: Graphics): Unit =
    val g2d : Graphics2D = g.asInstanceOf[Graphics2D]
    for ((x,y) <- stage.hexes.keys)
    {
      val hexContents = stage.hexes((x,y)).contents.toArray
      Sorting.quickSort(hexContents)(Ordering[Int].on((p:GameObject)=>p.zIndex))

      for (cont <- hexContents)
      {
        val img = new ImageIcon(cont.spriteFile)
        val viewCoords = Utils.getScreenXY(x,y,img.getIconWidth,img.getIconHeight)
        g2d.drawImage(img.getImage,viewCoords._1,viewCoords._2,this)
      }
    }

  private def nextLevel: Boolean =
    this.stage.level.nextLevel

  override def actionPerformed(actionEvent: ActionEvent): Unit =
    if (!stage.gameOver)
    {
      val lev = this.stage.level

      lev.takeTurn()
      stage.moveActors()
      repaint()
    }
    else
    {
      val rand = Random()
      var hextodrop = (0,0)
      val hexestodrop = stage.hexes.toList.filter(a =>
        !gameoverHexes.contains(a._1) && !hexesClearedForGameOver.contains(a._1)).map(h => h._1)
      val iter = rand.shuffle(hexestodrop).grouped(25)
      if (iter.hasNext)
      {
        for {hx <- iter.next()} {
          stage.hexes(hx).contents.foreach(f => if (!f.isInstanceOf[Void]) {
            stage.hexes(hx).removeContent(f)
          })
          hexesClearedForGameOver += hx
        }
        repaint()

      } else {
        timer.stop()
      }
    }
