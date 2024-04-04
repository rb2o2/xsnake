package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class SnakeBody() extends SnakeChunk:
  override val passable: Boolean = false
  val isHead = false

  override val spriteFile: String = Config.RESOURCE_PATH + "snake4_28x32.png"
