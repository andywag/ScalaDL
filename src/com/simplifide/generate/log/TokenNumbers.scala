package com.simplifide.generate.log

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/22/11
 * Time: 10:08 AM
 * To change this template use File | Settings | File Templates.
 */

class TokenNumbers(val count:Map[String,Int])  {

  def + (number:TokenNumbers) = {
    val list = count.toList ++ number.count.toList
    val merged = list.groupBy ( _._1) .map { case (k,v) => k -> v.map(_._2).sum }
    new TokenNumbers(merged)
  }

  def filter(length:Int) =
    new TokenNumbers(count.filter(x => x._2 > length))

  def debug:String =
    count.toList.sortBy(x => -x._2).map(x => x._1 + " -> " + x._2 + "\n").reduceLeft(_+_)


}

object TokenNumbers {
  def apply(strings:List[String]):TokenNumbers = {
    new TokenNumbers(strings.groupBy(x => x.toString).map(x => (x._1,x._2.length)))
  }

  //class TokenCount(item:String, List)
}