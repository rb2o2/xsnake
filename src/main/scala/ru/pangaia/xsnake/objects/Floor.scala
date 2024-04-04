package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.Config

import scala.util.Random

case class Floor() extends GameObject:
  override val zIndex: Int = 1
  override val passable: Boolean = true

  override def toString: String = ". "

  val r: Int = new Random().nextInt(3)

  override val spriteFile: String = Config.RESOURCE_PATH + "floor_" + r + "_28x32.png"
