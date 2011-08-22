package com.simplifide.generate.blocks.statemachine

import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.parser.condition.Case.State
import com.simplifide.generate.parser.block.state.State.Transition
import com.simplifide.generate.util.FileOps
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/22/11
 * Time: 8:19 AM
 * To change this template use File | Settings | File Templates.
 */

object DotGenerator {

  def createDotContents(model:StateModel):String = {
    def edgeCommand(transition:Transition):String =
      "   " + transition.source + " -> " + transition.destination + " [label=" + transition.connection + "];\n"
    val builder = new StringBuilder
    builder.append("diagraph state {\n")
    model.edges.foreach(builder.append(edgeCommand(_)))
    builder.append("}\n\n")

    builder.toString

  }

  def createDotFile(file:String, model:StateModel) = {
    FileOps.createFile(new File(file),createDotContents(model))
  }

}