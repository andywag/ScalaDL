package com.simplifide.generate.log

import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/21/11
 * Time: 10:02 AM
 * To change this template use File | Settings | File Templates.
 */

trait Node {

  val text:String
  val children:List[Node]
  val token:ParserTokens

  val paths:Int = if (children.size > 0) children.map(_.paths).reduceLeft(_+_) else 1

  def walk(indent:String):String = {
    def childString = children.size match {
      case 0 => "\n"
      case 1 => children.map(x => x.walk(indent)).reduceLeft(_+_)
      case _ => "\n" +  children.map(x => x.walk(indent + "  ")).reduceLeft(_+_)
    }
    indent + token.walkString + childString
  }

  /*
  def groupNodes:List[Node.Group] = {
    if (children.size == 1) List(new Node.Group(List(this,children(0))))
    else children.flatMap(_.groupNodes)
  }
  */

  def calculateKeyWords(probabilities:TokenNumbers):List[String] = {
    children.flatMap(_.calculateKeyWords(probabilities)) ::: token.calculateKeyWords(probabilities)
  }

 
  def tokenProbabilities:TokenNumbers  = {
    if (children.size > 0) children.map(x => x.tokenProbabilities).reduceLeft(_+_) + new TokenNumbers(Map(text -> paths))
    else new TokenNumbers(Map(text -> 1))

  }
  

  /** Find all nodes which begin with the string root */
  def findRoot(root:String):List[Node] = {
    if (text == root) return List(this) else children.flatMap(_.findRoot(root))
  }
  /** Merge this root together */
  def merge:Node = {
    val newChildren = children.groupBy(x => x.text).map(x => new Node.Item(x._2(0).token,x._2.flatMap(_.children.map(x => x.merge))))
    new Node.Item(this.token,newChildren.toList)
  }
  /** Merge the data together */
  def subRoot(root:String):Node = {
    val newRoot = new Node.Item(new ParserTokens.Identifier(root),findRoot(root).flatMap(x => x.children)) // Create a top node with the merged children
    newRoot.merge
  }
  
  def createGroupList:List[Node] = {
    if (this.token.isInstanceOf[ParserTokens.Identifier] &&
      this.children.size == 1 && children(0).token.isInstanceOf[ParserTokens.Identifier]) List(children(0)) ::: children(0).createGroupList
    else List()
  }
  
  def group:Node = {
    val combine = this.createGroupList
    if (combine.size >= 1) new Node.Item(new ParserTokens.GroupIdentifier(List(this.token) ::: combine.map(x => x.token)),combine.last.children.map(_.group))
    else new Node.Item(this.token,this.children.map(_.group))
  }



  def compress:Node = {
    this
  }
  
}

object Node {

  class Item(override val token:ParserTokens,override val children:List[Node]) extends Node {
    val text = token.text

  }

  class Group(nodes:List[Node])
  
  /*
  class Group(nodes:List[Node]) extends Node {
    val text = nodes.map(_.text + " ").reduceLeft(_+_)
    val children = nodes(nodes.size-1).children

    override def groupNodes:List[Node.Group] = {
      if (children.size == 1) new Node.Group(nodes ::: List(children(0))).groupNodes
      else return List(this) ::: this.groupNodes
    }

  }
  */
  




}