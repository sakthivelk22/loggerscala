package org.beasts.logging

import org.junit._
import Assert._

import Logger._

@Test
class AppTest {

    @Test
    def testOK() = {

        setDefaultLevel(Level.DEBUG)
        setWriteToConsole(true)
        log(Level.DEBUG,this.getClass,"this is a test message1")
        log(Level.INFO,this.getClass,"this is a test message2")
        addFileHandler("test.log")
        log(Level.ERROR,this.getClass,"this is a test message3")
        log(Level.ERROR,this.getClass,"this is a test message4")
        setWriteToConsole(false)
        addFileHandler("Console.log",Level.ERROR)
        log(Level.ERROR,this.getClass,"this is a test message5")
        log(Level.DEBUG,this.getClass,"this is a test message5")
        assertTrue(true)

    }

}


