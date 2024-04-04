package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class Wall() extends GameObject:
  override val zIndex: Int = 1000
  override val passable: Boolean = false

  override def toString: String = "# "

  override val spriteFile: String = Config.RESOURCE_PATH + "wall_28x32.png"
