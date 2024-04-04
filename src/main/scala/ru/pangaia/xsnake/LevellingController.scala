package ru.pangaia.xsnake

class LevellingController(l: Int) :
  var level0: Int = l
  var turnIndex: Int = 0
  var levelDuration: Int = Config.LEVEL0_DURATION

  val levelBoundaries: Seq[Int] = {
    LazyList.from(1).map(p => p * p * Config.LEVEL0_DURATION / Config.INITIAL_TIMER_DELAY)
      .take(30)
      .toList
  }

  def nextLevel: Boolean = {
    levelBoundaries.contains(turnIndex)
  }

  def takeTurn(): Unit = {
    turnIndex += 1
  }

  def getDelayForLevel(l: Int): Int = {
    if (Config.INITIAL_TIMER_DELAY / l >= Config.MIN_TIMER_DELAY) {
      Config.INITIAL_TIMER_DELAY / l
    }
    else Config.MIN_TIMER_DELAY
  }

