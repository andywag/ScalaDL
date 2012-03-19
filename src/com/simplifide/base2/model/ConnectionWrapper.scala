package com.simplifide.base2.model

import com.simplifide.base.core.instance.ModInstanceConnect
import scala.collection.JavaConverters._
import com.simplifide.base2.generator.ScalaDeclaration._
import com.simplifide.base2.generator.FunctionCall._
import com.simplifide.base2.generator.{ScalaDeclaration, FunctionCall}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/5/12
 * Time: 1:52 PM
 * To change this template use File | Settings | File Templates.
 */

class ConnectionWrapper(val connection:ModInstanceConnect) {

  val name         = connection.getname()
  val instanceName = connection.getConnectionName

  def ports       = connection.getPorts.asScala.map(new PortWrapper(_)).toList
  def parameters  = connection.getParameters.asScala.map(new PortWrapper(_)).toList

  def createScalaDeclaration = {
    ScalaDeclaration(instanceName,FunctionCall("instance",List(name.capitalize,"\"" + instanceName + "\"")))

  }

}