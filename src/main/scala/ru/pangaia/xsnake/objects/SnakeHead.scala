package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.{Config, Utils}

case class SnakeHead() extends SnakeChunk:
  override val passable: Boolean = false
  val isHead: Boolean = true
  var direction: Int = Utils.RIGHT

  def withDirection(dir: Int): SnakeHead =
    direction = dir
    this

  override val spriteFile: String = Config.RESOURCE_PATH + "snake5_28x32.png"
