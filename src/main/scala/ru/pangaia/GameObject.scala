package ru.pangaia

import java.awt.Image
import javax.swing.ImageIcon

import scala.collection.mutable

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

  override val spriteFile: String = Config.RESOURCE_PATH  + "floor_28x32.png"
}

case class Wall() extends GameObject
{
  override val zIndex: Int = 1000
  override val passable: Boolean = false

  override def toString: String = "# "

  override val spriteFile: String = Config.RESOURCE_PATH + "wall_28x32.png"
}

case class Void() extends GameObject
{
  override val zIndex: Int = -1
  override val passable: Boolean = false

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

case class GreenApple(bonusPts: Integer) extends GameObject
{
  override val zIndex: Int = 2
  override val passable: Boolean = true
  override val spriteFile: String = Config.RESOURCE_PATH + "apple_green_28x32.png"
}

class Snake
{
  val chain:mutable.MutableList[SnakeChunk] = new mutable.MutableList[SnakeChunk]()


}