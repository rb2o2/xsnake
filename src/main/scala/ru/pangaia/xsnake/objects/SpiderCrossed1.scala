package ru.pangaia.xsnake.objects

import ru.pangaia.xsnake.{Config, Utils}

case class SpiderCrossed1(name: String) extends Enemy:
  override val zIndex: Int = 5
  var direction: Int = Utils.RIGHT
  override val passable: Boolean = false
  override val spriteFile: String = Config.RESOURCE_PATH + "spider_cross1_28x32.png"

  def withDirection(dir: Int): SpiderCrossed1 =
    direction = dir
    this
