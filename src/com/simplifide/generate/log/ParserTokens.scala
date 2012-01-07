package com.simplifide.generate.log

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/21/11
 * Time: 10:11 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class ParserTokens(val text:String) {

  def typeString:String
  def walkString = typeString

  val comparison = typeString

  def calculateKeyWords(probabilities:TokenNumbers):List[String] = List()

}

object ParserTokens {
  
  class IdentifierKey(text:String) extends ParserTokens(text) {
    def typeString:String = text
  }
  
  class Identifier(text:String, items:TokenNumbers = new TokenNumbers(Map())) extends ParserTokens(text) {
    def typeString:String = "identifier"
    override def walkString:String = 
      if (items.count.size == 0) text
      else if (items.count.size == 1) "identifier (" + items.count.keys.head + "(" + items.count.values.head + "))"
      else "identifer(" + items.count.size + ")"

    override def calculateKeyWords(probabilities:TokenNumbers):List[String] = {
      List()
    }
  }
  
  class GroupIdentifier(tokens:List[ParserTokens]) extends ParserTokens("Group") {
    def typeString:String = "group"

  }
  

  class Key(text:String)        extends ParserTokens(text) {
    def typeString:String = text
    override val comparison = text

  }
  class Int(text:String)         extends ParserTokens(text) {
    def typeString:String = "integer"
    override val comparison = "integer"
  }

  class StringToken(text:String) extends ParserTokens(text) {
    def typeString:String = "string"

  }

  class Error(text:String) extends ParserTokens(text) {
    def typeString:String = "error"

  }
  object EOF extends ParserTokens("EOF") {
    def typeString:String = "eof"
  }



}