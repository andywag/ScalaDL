package com.simplifide.generate.blocks.statemachine

import com.simplifide.generate.language.ExtraFile
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.parser.block.state.State.Transition
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.parser.block.state.State
import com.simplifide.generate.html.{Description, HtmlUtilities, HtmlTable}
import com.simplifide.generate.language.Conversions._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/22/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 */

class StateHtmlFile(override val filename:String,val model:StateModel) extends ExtraFile {

    val transitionContents = {
       def row(transition:Transition):List[Description] = List(transition.source.name,
         transition.destination.name,
         transition.expr.get.createCode(CodeWriter.Verilog).code,
         transition.comment.get)

       val head:List[Description] = List("Source","Destination","Condition","Description")
       val body:List[List[Description]] = model.edges.map(x => row(x))
       val htmlModel = new HtmlTable(head,body,Some("Transition Table"))
       htmlModel.createTable
    }

     val stateContents = {
       def row(state:State):List[Description] = List(state.name,
         state.index.toString,
         state.description.get)

       val head:List[Description] = List("State","Value","Description")
       val body:List[List[Description]] = model.groups.keys.toList.sortBy(_.index).map(x => row(x))
       val htmlModel = new HtmlTable(head,body,Some("State Description Table"))
       htmlModel.createTable
    }


    val contents = HtmlUtilities.fullHtml(filename + " State Machine Description",List(stateContents,transitionContents)).toString()

}