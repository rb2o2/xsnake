package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class WallCracked1() extends GameObject:
  override val zIndex: Int = 900
  override val passable: Boolean = false

  override def toString: String = super.toString

  override val spriteFile: String = Config.RESOURCE_PATH + "wall_cracked1_28x32.png"
