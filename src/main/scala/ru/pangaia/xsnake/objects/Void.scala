package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

case class Void() extends GameObject:
  override val zIndex: Int = -1
  override val passable: Boolean = true

  override def toString: String = "_ "

  override val spriteFile: String = Config.RESOURCE_PATH + "void_28x32.png"
