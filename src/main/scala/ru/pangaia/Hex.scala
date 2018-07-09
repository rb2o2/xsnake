package ru.pangaia

import scala.collection.mutable

/**
  * Created by oneuro on 03.02.17.
  */
trait PlaneVec
{
  def x: Int

  def y: Int

  def +(vec: PlaneVec): PlaneVec
}

case class VectorImpl(x: Int, y: Int) extends PlaneVec
{
  override def +(vec: PlaneVec): PlaneVec = VectorImpl(x + vec.x, y + vec.y)
}

case class Hex(x: Int, y: Int) extends PlaneVec
{
  def isPassable: Boolean =
  {
    contents.contains(Floor()) && !contents.contains(SnakeBody()) && !contents.contains(Wall())
  }

  var contents: mutable.MutableList[GameObject] = mutable.MutableList[GameObject](Void())

  override def +(vec: PlaneVec): Hex =
  {
    val z = y
    val s = x
    var newS = x + vec.x
    var newZ = if (y % 2 == 0) y + vec.y else y + vec.y
    Hex(newS, newZ)
  }

  def addContent(o: GameObject): Hex =
  {
    if (contents.filter(p => p.zIndex == o.zIndex).toList.isEmpty)
      this.contents += o
    this
  }

  def removeContent(o:GameObject): Hex =
  {
    this.contents = this.contents.filter((p:GameObject) => p.zIndex != o.zIndex)
    this
  }


}
