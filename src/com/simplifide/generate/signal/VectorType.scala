package com.simplifide.generate.signal

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class VectorType(val arr:List[Int], val reg:Option[Int]) {
   def slice:VectorType = new VectorType(arr.slice(1,arr.size-1),reg)
}

object VectorType {

  def newVector(len:Int) = new VectorType(List(len),None)
  object NoVector extends VectorType(List[Int](),None)
  
}