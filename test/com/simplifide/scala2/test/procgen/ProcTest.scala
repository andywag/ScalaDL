package com.simplifide.scala2.test.procgen

import com.simplifide.generate.blocks.basic.flop.ClockControl

import com.simplifide.generate.proc.{Delay, ProcProgram, Mux}
import com.simplifide.generate.TestConstants
import com.simplifide.generate.project.{Entity, Project, Module}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/15/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */

class ProcTest {

}

object ProcTest {

   object Proj extends Project {
     val location:String = TestConstants.locationPrefix + "procgen\\proc_output"

     implicit val clk = ClockControl("clk","reset")

     override val root = new Ent("proc")
   }

   class Ent(name:String)(implicit clk:ClockControl) extends Entity.Root(name,name) {
     override def createModule = {
        val mod = new Body("body").createModule
        val prog = new Program(new Body("body"))
        prog.parse
        prog.controls
        mod
     }
   }

   class Body(name:String)(implicit clk:ClockControl) extends Module(name) {

    val n = clk
    val wid = S(8,6)

    val in = this.signal("in",INPUT,wid)
    val R0 = register("R0",REG,wid)(1)
    val R1 = register("R1",REG,wid)(1)

    val X  = signal("X",WIRE,wid)
    val Y  = register("Y",REG,wid)(1)
    val Z  = register("Z",WIRE,wid)(1)

    val ctrl1 = signal("ctrl1",WIRE,S(2,0))
    val ctrl2 = signal("ctrl2",WIRE,S(2,0))
    val ctrl3 = signal("ctrl3",WIRE)
    val ctrl4 = signal("ctrl4",WIRE)
    val ctrl5 = signal("ctrl5",WIRE)


    X  := Mux(ctrl1,in,R0,R1)
    Y  := Mux(ctrl2,in,R0,R1)  * X
    Z  := Mux(ctrl3,X,Delay(Z,1)) + Delay(Y,1)
    R0 := Mux(ctrl4,in,Delay(Z,1))
    R1 := Mux(ctrl5,in,Delay(Z,1))

  }

  class Program(val body:Body)(implicit clk:ClockControl) extends ProcProgram(body) {
    import body._
    Z~>(1) <:= X +  R0 * R1; R0~>(0) <:= in
    Z~>(2) <:= Z +  R0 * R1; R1~>(1) <:= in

    /*for (n <- 2 until 31) {
      Z(n) <:=  Z(n-1) + R0*R1
    }*/
  }


  def main(args:Array[String]) =  {
    ProcTest.Proj.createProject2
  }
}
