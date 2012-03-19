package com.simplifide.base2.model

import com.simplifide.base.core.module.InstanceModule
import com.simplifide.generate.util.FileOps
import java.io.File
import com.simplifide.generate.signal.SignalTrait
import scala.collection.JavaConverters._
import com.simplifide.base2.project.{SuiteGenerator, ProjectGenerator}
import com.simplifide.base2.model.Module.Derived
import com.simplifide.base2.generator._

/**
 */

trait Module {

  /** Prefix to be used for generating this module*/
  val prefix:String
  /** Name of this module */
  val name:String
  /** Signals Contained in this Module*/
  val signals:List[SystemVarWrapper] 
  /** Instances Contained in this Module */
  val connections:List[ConnectionWrapper] 
  
  def location:Option[File] = None


  def create(location:java.io.File) = {
    val newClass = new Module.Scala(this)
    val newFile = ScalaFile(new File(location,name.capitalize + ".scala"),prefix,List(),List(newClass))
    newFile.createFile
  }

  




}

object Module {
  
  def apply(instanceModule:InstanceModule,prefix:String) = {
    val vars        = instanceModule.getAllVars.asScala.map(new SystemVarWrapper(_)).toList 
    val connections = instanceModule.getRealConnections.asScala.map(new ConnectionWrapper(_)).toList
    new Derived(prefix,instanceModule,vars,connections)
  }

  class Derived(val prefix:String,
    val instanceModule:InstanceModule,
    val signals:List[SystemVarWrapper],
    val connections:List[ConnectionWrapper]) extends Module {
    
    val name = instanceModule.getname()
    override def location = {
      Some(new File(instanceModule.createReferenceItem().getLocation.getUri.toURL.getFile))
    }
  }

  class Scala(val instance:Module) extends ScalaClass.Object {

    override val className = instance.name
    override val parent:String = "EntityParser"
    override val items = {
       List(moduleName) ::: List(moduleLocation) ::: instance.signals.map(_.createScalaDeclaration) ::: instance.connections.map(_.createScalaDeclaration)
    }
    override val imports = {
      List(new ScalaImport("com.simplifide.generate.parser.EntityParser"))
    }

    def moduleName     = ScalaDeclaration.Override("name",new ScalaSimple.String("\"" + className + "\""))
    def moduleLocation = ScalaDeclaration.Override("location",
      new ScalaSimple.String(instance.location match {
        case Some(x) => "Some(new java.io.File(\"" + x.getAbsolutePath  + "\"))".replaceAll ("\\\\","\\\\\\\\")
        case _       => "None"
      })
    )

  }
}
