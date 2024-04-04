package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class SpiderSilkAligned(dirBegin: Int, dirEnd: Int) extends GameObject:
  override val zIndex: Int = 4
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH + "web_" + dirBegin + "_" + dirEnd + "_28x32.png"
