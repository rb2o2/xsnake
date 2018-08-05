package ru.pangaia

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Sorting}

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
  var enemies: mutable.Map[String, Enemy] = new mutable.HashMap[String,Enemy]()
  var gameOver = false

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
  def moveSnake() : Unit =
  {
    val tailCoord = snake.last
    val headCoord = snake.head
    val onlyHead = tailCoord == headCoord
    val snakeHead = hexes(headCoord).contents.find(p=>p.isInstanceOf[SnakeHead]).get.asInstanceOf[SnakeHead]
    val dir = snakeHead.direction
    val nextHexCoord = Utils.getNeighboringCoords(headCoord,dir)
    if (hexes.keySet.contains(nextHexCoord) && hexes(nextHexCoord).isPassable)
    {
      val nxt = hexes(nextHexCoord).contents.filter(_.isInstanceOf[Fruit]).toList
      var ate = 0
      if (nxt.nonEmpty)
      {
        this.score += nxt(0).asInstanceOf[Fruit].bonusPts
        println(score)
        removeFromHex(nextHexCoord._1, nextHexCoord._2, nxt(0).asInstanceOf[Fruit])
        putToHex(nextHexCoord._1, nextHexCoord._2, SnakeHead().withDirection(dir))
        removeFromHex(headCoord._1, headCoord._2, SnakeHead())
        putToHex(headCoord._1, headCoord._2, SnakeBody())
        snake = nextHexCoord +=: snake
        eatFruit
        ate += 1
      }
      if (nxt.isEmpty && tailCoord != headCoord)
      {
        removeFromHex(tailCoord._1, tailCoord._2, SnakeBody())
        snake = snake.dropRight(1)
        removeFromHex(headCoord._1, headCoord._2, SnakeHead())
        putToHex(headCoord._1, headCoord._2, SnakeBody())
        putToHex(nextHexCoord._1, nextHexCoord._2, SnakeHead().withDirection(dir))
        snake = nextHexCoord +=: snake
      } else if (tailCoord == headCoord && nxt.isEmpty)
      {
        putToHex(nextHexCoord._1, nextHexCoord._2, SnakeHead().withDirection(dir))
        removeFromHex(headCoord._1, headCoord._2, SnakeHead())
        snake = snake.dropRight(1)
        snake = nextHexCoord +=: snake
      }
      ate -= 1
    }
    else if (hexes.contains(nextHexCoord) && !hexes(nextHexCoord).isPassable) {
      if (hexes(nextHexCoord).contents.exists(_.isInstanceOf[Wall]))
      {
        if (tailCoord != headCoord)
        {
          val tail = snake.last
          removeFromHex(tail._1,tail._2, SnakeBody())
          snake = snake.dropRight(1)
        var wall = hexes(nextHexCoord).contents.find(_.isInstanceOf[Wall]).get
        removeFromHex(nextHexCoord._1,nextHexCoord._2, wall)
        //        hexes(nextHexCoord).contents = hexes(nextHexCoord).contents.filterNot(_.isInstanceOf[Wall])
        hexes(nextHexCoord).addContent(WallCracked1())
        }
        else {
          removeFromHex(snake.head._1, snake.head._2, SnakeHead())
          hexes(snake.head).addContent(Bang())
          endgame()
        }
      }
      else if (hexes(nextHexCoord).contents.exists(_.isInstanceOf[WallCracked1]))
      {
        if (tailCoord != headCoord)
        {
          val tail = snake.last
          removeFromHex(tail._1,tail._2, SnakeBody())
          //          hexes(tail._1,tail._2).contents = hexes(tail._1,tail._2).contents.filterNot(_.isInstanceOf[WallCracked1])
          snake = snake.dropRight(1)
        val crWall = hexes(nextHexCoord).contents.find(_.isInstanceOf[WallCracked1])
        removeFromHex(nextHexCoord._1, nextHexCoord._2, crWall.get)
        hexes(nextHexCoord).addContent(new WallCracked2)
        } else {
          removeFromHex(snake.head._1, snake.head._2, SnakeHead())
          hexes(snake.head).addContent(Bang())
          endgame()
        }

      }
      else if (hexes(nextHexCoord).contents.exists(_.isInstanceOf[WallCracked2]))
      {
        if (tailCoord != headCoord)
        {
          val tail = snake.last
          removeFromHex(tail._1,tail._2, SnakeBody())
          snake = snake.dropRight(1)
        val cr2Wall = hexes(nextHexCoord).contents.find(_.isInstanceOf[WallCracked2]).get
        removeFromHex(nextHexCoord._1,nextHexCoord._2,cr2Wall)
        } else {
          removeFromHex(snake.head._1, snake.head._2, SnakeHead())
          hexes(snake.head).addContent(Bang())
          endgame()
        }

      }
    }
  }

  def moveActors(): Unit =
  {
    moveSnake()
    for {s <- enemies.values} moveEnemy(s)
  }

  def endgame(): Unit = {
    gameOver = true
  }


  def moveEnemy(e: Enemy) : Unit =
  {
    val b: ListBuffer[(SpiderCrossed1, Hex)] = ListBuffer()
    var h: Hex = null
    for (h <- hexes.keySet;
         c <- hexes(h).contents) {
      if (c.isInstanceOf[SpiderCrossed1])
      {
        b += ((c.asInstanceOf[SpiderCrossed1], hexes(h)))
      }
    }

    for (a <- b) {
      a._2.removeContent(a._1)
      val weblist = for (neighbor <- getHexNeighbors((a._2.x,a._2.y))
                         if neighbor.contents.contains(SpiderSilkWeb())) yield neighbor
      val index = Random.nextInt(weblist.size)
      weblist(index).addContent(a._1)
    }
  }




  def looseTail() : Unit = {

  }

  def eatFruit(): Unit = {
    val bonuses = List(GreenApple(500), Orange(1000))
    val bonus = Random.shuffle(bonuses).head
    var s = -100
    var z = -100
    while(hexes.get((s,z)) == None || hexes((s,z)).contents.size > 2) {
      s = Random.nextInt(Config.maxS)
      z = Random.nextInt(Config.maxZ)
    }
    putToHex(s,z,bonus)
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