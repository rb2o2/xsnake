package ru.pangaia

import java.awt.Image
import javax.swing.ImageIcon

import scala.collection.mutable
import scala.util.Random

/**
  * Created by oneuro on 03.02.17.
  */
trait GameObject
{
  val zIndex: Int
  val passable: Boolean
  val spriteFile: String
}

case class Floor() extends GameObject
{
  override val zIndex: Int = 1
  override val passable: Boolean = true

  override def toString: String = ". "

  val r: Int = new Random().nextInt(3)

  override val spriteFile: String = Config.RESOURCE_PATH  + "floor_" + r + "_28x32.png"
}

case class Wall() extends GameObject
{
  override val zIndex: Int = 1000
  override val passable: Boolean = false

  override def toString: String = "# "

  override val spriteFile: String = Config.RESOURCE_PATH + "wall_28x32.png"
}

case class WallCracked1() extends GameObject
{
  override val zIndex: Int = 900
  override val passable: Boolean = false
  override def toString: String = super.toString

  override val spriteFile: String = Config.RESOURCE_PATH +  "wall_cracked1_28x32.png"
}

case class WallCracked2() extends GameObject
{
  override val zIndex: Int = 800
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH +  "wall_cracked2_28x32.png"
}

case class Void() extends GameObject
{
  override val zIndex: Int = -1
  override val passable: Boolean = true

  override def toString: String = "_ "

  override val spriteFile: String = Config.RESOURCE_PATH + "void_28x32.png"
}
abstract class SnakeChunk extends GameObject {
  override val zIndex: Int = 2
}

case class SnakeBody() extends SnakeChunk
{
  override val passable: Boolean = false
  val isHead = false

  override val spriteFile: String = Config.RESOURCE_PATH + "snake4_28x32.png"
}

case class SnakeHead() extends SnakeChunk
{
  override val passable: Boolean = false
  val isHead: Boolean = true
  var direction: Int = Utils.RIGHT

  def withDirection(dir:Int): SnakeHead =
  {
    direction = dir
    this
  }
  override val spriteFile: String = Config.RESOURCE_PATH + "snake5_28x32.png"

}

case class Sprite(x:Int, y:Int, img:Image,w:Int,h:Int,visible:Boolean)

trait Fruit extends GameObject
{
  def bonusPts: Integer
}

case class GreenApple(bonusPts: Integer) extends Fruit
{

  override val zIndex: Int = 2
  override val passable: Boolean = true
  override val spriteFile: String = Config.RESOURCE_PATH + "apple_green_28x32.png"
}
case class Orange(bonusPts: Integer) extends Fruit
{
  override val zIndex: Int = 2
  override val passable: Boolean = true
  override val spriteFile: String = Config.RESOURCE_PATH + "orange_28x32.png"
}

class Snake
{
  val chain:mutable.MutableList[SnakeChunk] = new mutable.MutableList[SnakeChunk]()
}

trait Enemy extends GameObject
{
  val name: String
  val path: Seq[Int] = Seq()//Int defines direction
}

case class SpiderCrossed1(name: String) extends Enemy
{
  override val zIndex: Int = 5
  var direction : Int = Utils.RIGHT
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH + "spider_cross1_28x32.png"

  def withDirection(dir: Int): SpiderCrossed1 =
  {
    direction = dir
    this
  }

}

case class SpiderSilkAligned(dirBegin : Int, dirEnd: Int) extends GameObject
{
  override val zIndex: Int = 4
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH + "web_" + dirBegin + "_" + dirEnd + "_28x32.png"
}