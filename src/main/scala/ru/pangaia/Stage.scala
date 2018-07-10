package ru.pangaia

import scala.collection.mutable
import scala.util.Sorting

/**
  * Created by oneuro on 03.02.17.
  */
class Stage(width: Int, height: Int)
{
  type Coord = (Int, Int)
  val hexes: mutable.HashMap[Coord, Hex] = new mutable.HashMap[Coord, Hex]()
  val rooms: mutable.MutableList[Room] = new mutable.MutableList[Room]()
  var snake: mutable.MutableList[Coord] = new mutable.MutableList[Coord]()
  var isRectangle = true
  var level: LevellingController = new LevellingController(1)
  var score: Int = 0

  def init: Unit =
  {
    for (x <- Range(0, width);
         y <- Range(0, height))
    {
      putToHex(x, y, Void())
      putToHex(x, y, Floor())
    }
  }

  def getHexNeighbors(coords: Coord): List[Hex] =
  {
    hexes.values.filter(h => Utils.isNeighborTo((h.x, h.y), coords)).toList
  }

  def removeFromHex(x: Int, y: Int, obj: GameObject): Unit =
  {
    hexes.getOrElse((x, y), Hex(x, y)).removeContent(obj)
  }

  def putToHex(x: Int, y: Int, obj: GameObject): Unit =
  {
    val old: Hex = hexes.get(x, y).getOrElse(Hex(x, y))
    hexes.put((x, y), old.addContent(obj))
  }

  def addSimpleSnake(bodyCoords: Coord*): Unit =
  {
    if (snake.nonEmpty)
    {
      throw new RuntimeException("snake already on stage")
    }
    val headCoord = bodyCoords.head
    addSnakeHead(headCoord)

    val possibleBody = bodyCoords.toArray
    var prev = headCoord
    for (c <- bodyCoords.tail)
    {
      if (!Utils.isNeighborTo(prev, c))
      {
        throw new RuntimeException("body chunks must be neighbors pairwise")
      }
      prev = c
      addSnakeBody(c)
    }
  }

  def addSnakeHead(c: Coord): Unit =
  {
    putToHex(c._1, c._2, SnakeHead())
    snake +== c
  }

  def addSnakeBody(c: Coord): Unit =
  {
    if (snake.isEmpty) throw new RuntimeException("must push head first")
    snake +== c
    putToHex(c._1, c._2, SnakeBody())
  }

  def getSnakeHead: SnakeHead =
  {
    hexes(snake(0)).contents.find(p=>p.isInstanceOf[SnakeHead]).get.asInstanceOf[SnakeHead]
  }

  def moveSnake(): Unit =
  {
    val tailCoord = snake.last
    val headCoord = snake(0)
    val snakeHead = hexes(headCoord).contents.find(p=>p.isInstanceOf[SnakeHead]).get.asInstanceOf[SnakeHead]
    val dir = snakeHead.direction
    val nextHexCoord = Utils.getNeighboringCoords(headCoord,dir)
    val tail = hexes(tailCoord)
    if (hexes.keySet.contains(nextHexCoord) && hexes(nextHexCoord).isPassable)
    {
      val nxt = hexes(nextHexCoord).contents.filter(_.isInstanceOf[GreenApple]).toList
      if (nxt.nonEmpty)
      {
        this.score += nxt(0).asInstanceOf[GreenApple].bonusPts
        println(score)
        removeFromHex(nextHexCoord._1, nextHexCoord._2, nxt(0).asInstanceOf[GreenApple])
      }
      removeFromHex(tailCoord._1, tailCoord._2, SnakeBody())
      removeFromHex(headCoord._1, headCoord._2, SnakeHead())
      putToHex(headCoord._1, headCoord._2, SnakeBody())
      putToHex(nextHexCoord._1, nextHexCoord._2, SnakeHead().withDirection(dir))
      snake = nextHexCoord +=: snake
      snake = snake.dropRight(1)
    }
  }

  def addRoom(top: Int, left: Int, width: Int, height: Int): Unit =
  {
    for (x <- Range(left, left + width + 1);
         y <- Range(top, top + height + 1)) putToHex(x, y, Floor())

    surroundByWalls(left, left + width + 1, top, top + height + 1)
    rooms += Room(top, left, width, height)

  }

  def surroundByWalls(minS: Int, maxS: Int, minZ: Int, maxZ: Int): Unit =
  {
    for (x <- Range(minS, maxS + 1))
    {
      putToHex(x, minZ, Wall())
      putToHex(x, maxZ, Wall())
    }
    for (y <- Range(minZ, maxZ + 1))
    {
      putToHex(minS, y, Wall())
      putToHex(maxS, y, Wall())
    }
  }

  override def toString: String =
  {
    var seq = hexes.keys.toArray
    Sorting.quickSort(seq)(Ordering[(Int, Int)].on(p => (p._2, p._1)))
    var out: mutable.StringBuilder = new mutable.StringBuilder()
    var prev = seq(0)._2 - 1
    var parity = false
    for (h <- seq)
    {
      if (h._2 != prev)
      {
        parity = !parity
        out.append(if (parity) "\n " else "\n")
      }
      out.append(hexes(h).contents.maxBy(x => x.zIndex).toString())
      prev = h._2
    }
    out.append("\n")
    out.toString()
  }
}

class LevellingController(l: Int)
{
  var level0: Int = l
  var turnIndex: Int = 0
  var levelDuration: Int = Config.LEVEL0_DURATION

  val levelBoundaries: Seq[Int] =
  {
    Stream.from(1).map((p) =>p*p*Config.LEVEL0_DURATION / Config.INITIAL_TIMER_DELAY)
      .take(30)
      .toList
  }

  def nextLevel: Boolean =
  {
    levelBoundaries.contains(turnIndex)
//    turnIndex % 100 == 0
  }


  def takeTurn(): Unit =
  {
    turnIndex += 1
  }

  def getDelayForLevel(l: Int): Int =
  {
    if (Config.INITIAL_TIMER_DELAY / l >= Config.MIN_TIMER_DELAY)
    {
      Config.INITIAL_TIMER_DELAY/l
    }
    else Config.MIN_TIMER_DELAY
//    500
  }
}

case class Room(top: Int, left: Int, width: Int, height: Int)