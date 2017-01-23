package org.beasts.logging

import java.util.Date
import java.text.SimpleDateFormat
import java.io._


/**
  * Created by SAK on 25-12-2016.
  */
object Level extends Enumeration {
  type Level = Value
  val ERROR, WARNING, LOG, DEBUG, INFO = Value
}

import Level._

object Logger {
  private var level: Level = Level.LOG
  private var handler: Map[String, Level] = Map[String, Level]()
  private var writeToConsole: Boolean = true

  def setDefaultLevel(level: Level): Unit = {this.level = level}

  def setWriteToConsole(toWrite: Boolean): Unit = {writeToConsole = toWrite}

  def log(logLevel: Level, identifier: Class[_], message: String): Unit = {

    def writeToHandler(x: String, level: Level, message: String): Unit = {
      val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(x, true)))
      if (logLevel >= level) {
        bw.write(message)
        bw.flush()
      }
    }

    val date = new Date()
    val format = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss:SSS")
    val messageString = format.format(date) + " - " + logLevel.toString  + " \r " + identifier.toString + " - " + message + "\r"
    if (handler.size > 0) handler.map { case (file, level) => writeToHandler(file, level, messageString) }
    if (writeToConsole) println(messageString)
  }

  def addFileHandler(filename:String,level:Level): Unit ={
    handler += (filename -> level)
  }

  def addFileHandler(filename:String): Unit ={
    handler += (filename -> this.level)
  }


}