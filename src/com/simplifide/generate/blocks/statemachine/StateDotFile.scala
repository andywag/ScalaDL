package com.simplifide.generate.blocks.statemachine

import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.parser.condition.Case.State
import com.simplifide.generate.parser.block.state.State.Transition
import com.simplifide.generate.util.FileOps
import java.io.File
import com.simplifide.generate.language.ExtraFile
import com.simplifide.generate.generator.CodeWriter

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/22/11
 * Time: 8:19 AM
 * To change this template use File | Settings | File Templates.
 */

class StateDotFile(override val filename:String,val model:StateModel) extends ExtraFile {

  val contents:String = {
    def edgeCommand(transition:Transition):String =
      "   " + transition.source.name + " -> " + transition.destination.name + " [label=\"" + transition.expr.get.createCode(CodeWriter.Verilog) + "\"];\n"
    val builder = new StringBuilder
    builder.append("digraph state {\n")
    model.edges.foreach(x => builder.append(edgeCommand(x)))
    builder.append("}\n\n")

    builder.toString

  }



}