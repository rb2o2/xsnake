package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class WallCracked2() extends GameObject:
  override val zIndex: Int = 800
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH + "wall_cracked2_28x32.png"
