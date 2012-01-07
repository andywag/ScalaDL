package com.simplifide.generate.log

import util.parsing.combinator.syntactical.StandardTokenParsers
import com.simplifide.generate.util.FileOps
import util.parsing.combinator.token.StdTokens
import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 12/20/11
 * Time: 6:20 PM
 * To change this template use File | Settings | File Templates.
 */

object LogParser extends StandardTokenParsers {

  lexical.delimiters ++= List(":","-","+","(",")","{","}","<",">",",",".","`","~","@","#","$","%","^","&","*","/","\\","=")



  def tokenize(text:String) = new lexical.Scanner(text)

  def compareScanners(scanners:Array[lexical.Scanner]) = {
    null
  }



  def convertTokenOld(scanner:lexical.Scanner,allKeys:Boolean = false):ParserTokens = {
    scanner.first match {
      case lexical.Keyword(x)     => new ParserTokens.Key(x)
      case lexical.Identifier(x)  => if (allKeys) new ParserTokens.IdentifierKey(x) else new ParserTokens.Identifier(x)
      case lexical.StringLit(x)   => new ParserTokens.StringToken(x)
      case lexical.NumericLit(x)  => new ParserTokens.Int(x)
      case lexical.ErrorToken(x)  => new ParserTokens.Error(x)
      case lexical.EOF            => ParserTokens.EOF
    }
  }

  def convertToken(scanner:List[lexical.Scanner],allKeys:Boolean = false):ParserTokens = {
    scanner(0).first match {
      case lexical.Keyword(x)     => new ParserTokens.Key(x)
      /*case lexical.Identifier(x)  => if (allKeys) new ParserTokens.IdentifierKey(x)
        else new ParserTokens.Identifier(x, scanner.map(x => x.first.chars).toList)*/
      case lexical.StringLit(x)   => new ParserTokens.StringToken(x)
      case lexical.NumericLit(x)  => new ParserTokens.Int(x)
      case lexical.ErrorToken(x)  => new ParserTokens.Error(x)
      case lexical.EOF            => ParserTokens.EOF
    }
  }


  def analyzeSub(root:Node) = {
    val probabilities = root.tokenProbabilities
    val counts = probabilities.count.toList.sortBy(x => -x._2).filter(x => x._2 > 100)
    val node1 = root.subRoot("<")
    System.out.println(node1.walk(""))
  }
  
  def createNode(lines:Array[String],allKeys:Boolean):Node = {
    val scanners        = lines.map(tokenize(_)).groupBy(x => convertTokenOld(x,allKeys).comparison)
    val nodes = scanners.map(x => createNode(convertToken(x._2.toList,allKeys),x._2.toList,allKeys))
    new Node.Item(new ParserTokens.Identifier("ROOT"),nodes.toList) 
  }
  
  def analyze(text:String):String = {
    val lines = text.split("\n")    

    lexical.reserved ++= List("WARNING","INFO")

    val probNode   = createNode(lines,true)
    val parserNode = createNode(lines,false)
    val probabilities = probNode.tokenProbabilities
    
    //probNode.walk("") + parserNode.walk("")
    //probabilities.debug
    parserNode.walk("")

    /*
    val scanners        = lines.map(tokenize(_)).groupBy(x => convertTokenOld(x).comparison)
    val nodes = scanners.map(x => createNode(convertToken(x._2.toList),x._2.toList))
    val rootNode = new Node.Item(new ParserTokens.Identifier("ROOT"),nodes.toList)
    val probs = rootNode.tokenProbabilities
    System.out.println(probs.debug)
    */

    //val newNodes = nodes.map(x => (if (x.paths > 40) x.compress else x) )
    //newNodes.map(x => x.walk("")).reduceLeft(_+_)


  }




  /** Create a tree full of nodes describing the input */
  def createNode(token:ParserTokens, scanners:List[lexical.Scanner],allKeys:Boolean):Node = {
    val newNodes1 = scanners.filter(x => !x.atEnd).map(x => x.rest).groupBy(x => convertTokenOld(x,allKeys).comparison)
    val newNodes  = newNodes1.map(x => createNode(convertToken(x._2.toList,allKeys),x._2,allKeys))
    new Node.Item(token, newNodes.toList)
  }







}