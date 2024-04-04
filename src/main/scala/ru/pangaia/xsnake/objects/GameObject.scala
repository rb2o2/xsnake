package ru.pangaia.xsnake.objects

trait GameObject:
  val zIndex: Int
  val passable: Boolean
  val spriteFile: String
