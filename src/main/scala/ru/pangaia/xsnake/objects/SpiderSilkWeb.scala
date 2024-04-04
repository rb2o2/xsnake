package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class SpiderSilkWeb() extends GameObject:
  override val zIndex: Int = 4
  override val passable: Boolean = true
  override val spriteFile: String = Config.RESOURCE_PATH + "web_circle1_28x32.png"
