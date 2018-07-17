package ru.pangaia

import java.awt.event.ActionListener
import javax.swing.Timer

object Config
{
  val maxS = 36
  val maxZ = 30
  val WIDTH = 1024
  val HEIGHT = 800
  val RESOURCE_PATH = "src/main/resources/"
  val INITIAL_TIMER_DELAY = 500
  val MIN_TIMER_DELAY = 30
  val LEVEL0_DURATION = 10000
}

class AcceleratingTimer(actionListener: ActionListener)
{
  var delay:Int = Config.INITIAL_TIMER_DELAY
  var timer:Timer = new Timer(delay, actionListener)

  def restartWithDelay(delay: Int): Unit =
  {
    this.delay = delay
    stop()
    timer.removeActionListener(actionListener)
    timer = new Timer(this.delay,actionListener)
    start()
  }

  def start() : Unit = {timer.start()}
  def stop() : Unit = {timer.stop()}

}

object Utils
{
  val RIGHT = 6
  val RIGHT_LOW = 1
  val LEFT_LOW = 2
  val LEFT = 3
  val LEFT_HI = 4
  val RIGHT_HI = 5

  def isNeighborTo(c1:(Int, Int),c2:(Int,Int)): Boolean =
  {
    var result = false
    val s = c1._1
    val z = c1._2
    val so = c2._1
    val zo = c2._2
    val parity = z % 2==0
    val diff = (s-so,z-zo)
    if (Math.abs(z-zo)>1 || Math.abs(s-so)>1) result = false
    if(parity)
    {
      result = diff match
      {
        case (-1,-1) | (-1,0) | (-1,1) | (0,-1) | (0,1) | (1,0) => true
        case _ => false
      }
    }
    else
    {
      result = diff match
      {
        case (-1,0) | (0,-1) | (0,1) | (1,-1) | (1,0) | (1,1) => true
        case _ => false
      }
    }
    result
  }

  def getNeighboringCoords(coord:(Int,Int), dir:Int): (Int,Int) =
  {
    val s = coord._1
    val z = coord._2
    val parity = z%2 == 0
    if (!parity)
      dir match
    {
      case RIGHT => (s+1,z)
      case LEFT => (s-1,z)
      case RIGHT_HI => (s,z-1)
      case RIGHT_LOW => (s,z+1)
      case LEFT_HI => (s-1,z-1)
      case LEFT_LOW => (s-1,z+1)
      case _ => throw new RuntimeException("direction must be 0<dir<7")
    }
    else
    {
      dir match
      {
        case RIGHT => (s+1,z)
        case LEFT => (s-1,z)
        case RIGHT_HI => (s+1,z-1)
        case RIGHT_LOW => (s+1,z+1)
        case LEFT_HI => (s,z-1)
        case LEFT_LOW => (s,z+1)
        case _ => throw new RuntimeException("direction must be 0<dir<7")
      }
    }
  }

  def getScreenXY(s:Int,z:Int,imgWidth:Int,imgHeight:Int): (Int,Int) =
  {
    val parity = z%2 == 0
    return (if (parity) imgWidth/2 + s*imgWidth else s*imgWidth,3*z*imgHeight/4)
  }
}