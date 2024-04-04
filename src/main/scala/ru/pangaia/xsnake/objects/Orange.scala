package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class Orange(bonusPts: Integer) extends Fruit:
  override val zIndex: Int = 2
  override val passable: Boolean = true
  override val spriteFile: String = Config.RESOURCE_PATH + "orange_28x32.png"
