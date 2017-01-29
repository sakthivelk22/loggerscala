package org.beasts.logging

import java.util.Date
import java.text.SimpleDateFormat
import java.io._
import scala.io.Source

/**
  * Created by SAK on 25-12-2016.
  * Updated by SAK on 29-01-2017
  */
object Level extends Enumeration {
  type Level = Value
  val INFO, DEBUG, LOG, WARNING, ERROR= Value
}

import Level._

object Logger {
  private var level: Level = Level.LOG
  private var handler: Map[String, Level] = Map[String, Level]()
  private var writeToConsole: Boolean = true
  private val propFile: String = "Logger.properties"

  println("Loading properties file : "+propFile)
  try {
    for (line <- Source.fromFile(propFile).getLines()) {
      setProps(line.split('='){0},line.split('='){1})
    }
  }
  catch {
    case ex: scala.MatchError =>
      println("Aborting properties file load. Unrecognised values found")
      ex.printStackTrace()
    case ex: Exception =>
      println("Unable to load properties file. Defaulting Logger properties")
      ex.printStackTrace()
  }

  private def setProps(key: String, value:String):Unit = {
    def toLevel(value:String):Level={
      value match {
        case "ERROR" => ERROR
        case "WARNING" => WARNING
        case "LOG" =>  LOG
        case "DEBUG" => DEBUG
        case "INFO" => INFO
      }
    }
    def toBoolean(value:String):Boolean={
      value match {
        case "TRUE" => true
        case "FALSE" => false
      }
    }
    key match {
      case "AddHandler" =>
        val values = value.split(',')
        if (values.length==3) this.addFileHandler(values{0},toLevel(values{1}),toBoolean(values{2}))
        else if (values.length==2) this.addFileHandler(values{0},toLevel(values{1}))
        else this.addFileHandler(values{0})
      case "DefaultLevel" => this.setDefaultLevel( toLevel(value))
      case "WriteToConsole" => this.setWriteToConsole(toBoolean(value))
    }
  }

  def setDefaultLevel(level: Level): Unit = {this.level = level}

  def setWriteToConsole(toWrite: Boolean): Unit = {writeToConsole = toWrite}

  def log(logLevel: Level, identifier: Class[_], message: String): Unit = {
    def writeToHandler(x: String, level: Level, message: String): Unit = {
      val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(x, true)))
      if (logLevel >= level) {
        bw.write(message)
        bw.flush()
      }
      bw.close()
    }
    val messageString = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss:SSS").format(new Date()) + " - " +
                             logLevel.toString  + " \r " + identifier.toString + " - " + message + "\r"
    if (handler.nonEmpty) handler.map { case (file, level) => writeToHandler(file, level, messageString) }
    if (writeToConsole && logLevel>=this.level) println(message)
  }

  def addFileHandler(filename:String,level:Level,append:Boolean): Unit ={
    handler += (filename -> level)
    if (!append) rollOverHandler(filename)
  }

  def addFileHandler(filename:String,level:Level): Unit ={ handler += (filename -> level) }

  def rollOverHandler(fileName:String):Unit ={
    if(handler.get(fileName).nonEmpty){
      println("Rolling over "+fileName)
      new File(fileName).renameTo(new File(fileName+ new SimpleDateFormat(".yyyy-MM-dd-HH-mm-ss-SSS").format(new Date())))
    }
  }

  def addFileHandler(filename:String): Unit ={ handler += (filename -> this.level) }

}