package com.simplifide.scala2.test.procgen

import com.simplifide.generate.blocks.basic.flop.ClockControl

import com.simplifide.generate.TestConstants
import com.simplifide.generate.blocks.basic.memory.Memory.MemoryBus
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.test.{Isim, Test, TestModule}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.blocks.basic.memory._
import com.simplifide.generate.blocks.basic.memory.Behavioral._
import com.simplifide.generate.signal.{SignalTrait, Constant}
import com.simplifide.generate.project._
import com.simplifide.generate.proc.blocks.{MacNormal, Mac, Delay, Mux}
import com.simplifide.generate.proc.{Controls, InstructionDecoder, ControlHTML, ProcProgram}
import com.simplifide.generate.proc.parser.ProcessorSegment
import com.simplifide.generate.blocks.basic.fixed.{AdditionSegment2, MultiplySegment}
import com.simplifide.generate.generator.{ComplexSegment, SimpleSegment}
import com.simplifide.scala2.test.procgen.FftProcTest.AluEntity.AluProgram
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/15/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */

class FftProcTest {

}

object FftProcTest {

   object Proj extends Project {
     val location:String = TestConstants.locationPrefix + "procgen" + TestConstants.separator + "proc_output"

     implicit val clk = ClockControl("clk","reset")

     override val root = new Ent("proc",location)
     override val tests:List[Test] = List(Test(new TestCase(root)))
     override val testType = Some(new Isim(this))

     val program = new Prog(root)

     def  debugControls = {
       /*
       val cont = root.instances.flatMap(_.createControls)
       cont.foreach(x => System.out.println(x))
       val mm = program.parse
       */
     }
   }

    /** Test Case for the Filter */
  class TestCase(val entity:Ent)(implicit clk:ClockControl) extends TestModule("test_iir",entity) {
    // Tap Values for numerator and denominator
    this.createTest
  }

   class Ent(name:String, location:String)(implicit clk:ClockControl) extends Entity.Root(name,name) {
     override val entitySignals = clk.allSignals(INPUT)

     val memoryModel = Behavioral(new MemoryModel("memory",16,256,2),1,1)
     val memory      = new MemoryEntity(memoryModel)

     // Internal Signal Declarations
     val instruction = signal("instruction"  ,WIRE,U(64,0))
     val aluControl0 = signal("alu_0_control",WIRE,U(5,0))
     val aluControl1 = signal("alu_1_control",WIRE,U(5,0))
     // Internal Memory Declarations
     val memory0Write = signal(memory.memory.writeBuses.map(x => x.copy("memory_0_write",WIRE)))
     val memory0Read  = signal(memory.memory.readBuses.map(x => x.copy("memory_0_read",WIRE)))
     val memory1Write = signal(memory.memory.writeBuses.map(x => x.copy("memory_1_write",WIRE)))
     val memory1Read  = signal(memory.memory.readBuses.map(x => x.copy("memory_1_read",WIRE)))


     val alu  = new AluEntity("alu",this)



     val memory0 = EntityInstance(memory,"memory_0",Connection.MapSignalConnection(Map(
       memory.memory.writeBuses(0) -> this.memory0Write,
       memory.memory.readBuses(0)  -> this.memory0Read
     )))
     val memory1 = EntityInstance(memory,"memory_1",Connection.MapSignalConnection(Map(
       memory.memory.writeBuses(0) -> this.memory1Write,
       memory.memory.readBuses(0)  -> this.memory1Read
     )))
     val alu0    = EntityInstance(alu,"alu_0",Connection.MapSignalConnection(Map(
       alu.control    -> aluControl0
     )))
     val alu1    = EntityInstance(alu,"alu_1",Connection.MapSignalConnection(Map(
       alu.control -> aluControl1
     )))

    def alu0Program(expression:Expression) = new AluProgram(alu0,expression)
    def alu1Program(expression:Expression) = new AluProgram(alu1,expression)


     val program = new Prog(this)
     val instructionDecoder = new InstructionDecoder("idecoder",instruction,program.instruction)

     override val instances = List(
       alu0,
       alu1,
       memory0,
       memory1,
       EntityInstance(instructionDecoder))


   }

  class AluEntity(name:String, parent:Ent)(implicit clk:ClockControl)
    extends Entity.Leaf(name,name) {


    val control     = signal("alu_control" ,INPUT ,U(5,0))
    val memoryOut0  = signal("memory_out_0",INPUT,U(16,0))
    val memoryOut1  = signal("memory_out_1",INPUT,U(16,0))
    val aluOut      = signal("alu_out",OUTPUT,U(16,0))

    // Mac Attachment
    val mac = new MacNormal(U(16,0))
    /- ("X input mux")
    mac.X    := Mux(control(5),memoryOut0,memoryOut1)
    /- ("Y input mux")
    mac.Y    := Mux(control(4),memoryOut0,memoryOut1)
    /- ("Output Assignment")
    aluOut   := mac.Z

    /- ("Mac ProcStatement")
    assign(mac.split)

    override val entitySignals = clk.allSignals(INPUT) ::: this.extraSignals
    override lazy val controls = List(Controls(control))

  }

  object AluEntity {
    class AluProgram(val entity:EntityInstance[AluEntity],val expression:SimpleSegment) extends SimpleSegment.Combo {
      override def createControl(actual:SimpleSegment,statements:ProcessorSegment,index:Int):List[Controls.Value] = {
        val cont = entity.entity.mac(expression).createControl(actual,statements,index)
        // Replace the Controls

        val cont1 = cont.map(x => x.connect(entity.connection))
        return cont1
      }
    }
  }

  class Prog(entity:Ent)(implicit clk:ClockControl) extends ProcProgram(entity,4) {
    import entity._

    memory1(0) <:= alu0Program(memory(0) * memory(8))
    //memory1(1) <:= alu.mac(alu.mac.multiplierOutput +  memory1(1) * memory2(9))
    //memory1(2) <:= alu.mac(alu.mac.multiplierOutput +  memory1(2) * memory2(10))

  }




  def main(args:Array[String]) =  {
    FftProcTest.Proj.createProject2
    FftProcTest.Proj.debugControls
  }
}
