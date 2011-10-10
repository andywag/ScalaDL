package com.simplifide.generate.util

/**
 * Created by IntelliJ IDEA.
 * User: Andy
 * Date: 9/26/11
 * Time: 11:25 PM
 * To change this template use File | Settings | File Templates.
 */

class ProcessOps {

}

object ProcessOps {



  def exec(cmd : String, dir:Option[String] = None)(func : String=>Unit) : Unit = {
	  val commands = cmd.split(" ")
    val proc1 = new ProcessBuilder(commands: _*)
    if (dir.isDefined) proc1.directory(new java.io.File(dir.get))
	  val proc = proc1.redirectErrorStream(true).start();
	  val ins = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream))
	  val sb = new StringBuilder

	  //spin off a thread to read process output.
	  val outputReaderThread = new Thread(new Runnable(){
		  def run : Unit = {
			  var ln : String = null
			  while({ln = ins.readLine; ln != null})
				  func(ln)
		  }
	  })
	  outputReaderThread.start()

	  //suspense this main thread until sub process is done.
	  proc.waitFor

	  //wait until output is fully read/completed.
	  outputReaderThread.join()

	  ins.close()
}

}