package ru.pangaia.xsnake

import java.awt.event.ActionListener
import javax.swing.Timer

class AcceleratingTimer(actionListener: ActionListener):
  var delay: Int = Config.INITIAL_TIMER_DELAY
  var timer: Timer = new Timer(delay, actionListener)

  def restartWithDelay(delay: Int): Unit =
    this.delay = delay
    stop()
    timer.removeActionListener(actionListener)
    timer = new Timer(this.delay, actionListener)
    start()

  def start(): Unit = {
    timer.start()
  }

  def stop(): Unit = {
    timer.stop()
  }
