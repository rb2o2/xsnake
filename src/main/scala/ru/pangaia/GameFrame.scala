package ru.pangaia

import java.awt._
import javax.swing.JFrame

/**
  * Created by oneuro on 03.02.17.
  */
class GameFrame extends JFrame
{
  def initUI(): Unit =
  {
    setSize(Config.WIDTH,Config.HEIGHT)
    setLocationRelativeTo(null)
    setResizable(false)

    setTitle("Xnake v.0.1")
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  initUI()
  add(new Board())
}

object GameFrame extends JFrame {
  def main(args: Array[String]): Unit =
  {
    EventQueue.invokeLater(() =>
    {
      val frame: GameFrame = new GameFrame()
      frame.setVisible(true)
    })
  }
}


