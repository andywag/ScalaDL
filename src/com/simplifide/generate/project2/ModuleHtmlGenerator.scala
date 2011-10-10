package com.simplifide.generate.project2

import com.simplifide.generate.language.ExtraFile
import com.simplifide.generate.parser.block.state.State
import com.simplifide.generate.html.{HtmlTable, Description, HtmlUtilities}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.signal.{RegisterTrait, ArrayTrait, SignalTrait}
import com.simplifide.generate.hier2.Entity

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/26/11
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */

class ModuleHtmlGenerator(override val filename:String, val module:Entity) extends ExtraFile {

  val head        = <h1>{module.name} Description</h1>
  val description = module.description match {
    case None    => <p></p>
    case Some(x) => x.html
  }

  val signalTable = {
    def typ(signal:SignalTrait) = {
      signal match {
        case x:RegisterTrait[_] => "Register(" + x.length + ")"
        case x:ArrayTrait[_]    => "Array(" +  x.length + ")"
        case x:SignalTrait   => x.opType.toString
        case _ => ""
      }
    }
    def row(signal:SignalTrait):List[Description] = List(signal.name,
      signal.fixed.toString,
      typ(signal),
      signal.description.getOrElse(""))

    val head:List[Description] = List("Name","Width","Type","Description")
       val body:List[List[Description]] = module.signals.map(x => row(x))
       val htmlModel = new HtmlTable(head,body,Some("State Description Table"))
       htmlModel.createTable
  }


  val contents = HtmlUtilities.fullHtml(filename + " Impl Description",List(head,description,signalTable)).toString()
}

object ModuleHtmlGenerator {
  
}