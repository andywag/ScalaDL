package com.simplifide.generate.signal

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class Signing {
  val isSigned:Boolean = false;
  val isControl:Boolean = false;
 
}

object Signing {
  object Signed extends Signing {
    override val isSigned:Boolean = true;
  }
  
  object UnSigned extends Signing
  
  object Control extends Signing {
    override val isControl:Boolean = true
  }
  
}
