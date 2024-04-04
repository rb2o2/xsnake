package ru.pangaia.xsnake.objects

import scala.collection.mutable

class Snake:
  val chain: mutable.ListBuffer[SnakeChunk] = new mutable.ListBuffer[SnakeChunk]()

 //Int defines direction







