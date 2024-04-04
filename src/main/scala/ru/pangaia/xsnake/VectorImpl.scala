package ru.pangaia.xsnake


case class VectorImpl(x: Int, y: Int) extends PlaneVec:
  override def +(vec: PlaneVec): PlaneVec = VectorImpl(x + vec.x, y + vec.y)
