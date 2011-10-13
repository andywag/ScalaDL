package com.simplifide.scala2.test.language

import com.simplifide.generate.project2.{Module, Project}
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.complex.ComplexSignal
import com.simplifide.generate.signal.{ArrayTrait, FixedType}
import com.simplifide.generate.parameter.{ModuleScope, Parameter}
import com.simplifide.generate.hier2.Entity
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.TestConstants

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/12/11
 * Time: 2:10 PM
 * To change this template use File | Settings | File Templates.
 */


// TODO : Need to Fix the Automatic Connections for this Block

/** FFT Project class which contains the list of modules and file locations */
class FftParamTest extends Project {
  // Creates a new Set of parameters for the test
  val parameters = new FftParamTest.FftMain.Params
  parameters.fftSize.set(5) // Changes the size of the FFT
  // Location of the Test
  val location:String = TestConstants.locationPrefix + "language" + TestConstants.separator + "fft_output"
  // Clock Generation
  implicit val clk = ClockControl("clk","reset")

    val signalIn  = complex_array("signal_in",INPUT,S(8,6))(parameters.fftSize)
    val signalOut = complex_array("signal_out",OUTPUT,S(12,10))(parameters.fftSize)

    override val root = new FftParamTest.FftMainEntity("fft",signalIn,signalOut,parameters)
}

object FftParamTest {

  /** FFT Top Level Block */
  class FftMainEntity(name:String,
                      val signalIn:ArrayTrait[ComplexSignal],
                      val signalOut:ArrayTrait[ComplexSignal],
                      val params:FftMain.Params)(implicit clk:ClockControl) extends Entity.Root(name,name) {

    import params._
    def butterfly(r:Int,c:Int):ButterflyEntity =
      new ButterflyEntity("butterfly_" + r + "_" + c,stageParams(r)(c))

    val fftLength = math.pow(2.0,fftSize.toDouble).toInt

    override val signals = List(signalIn,signalOut)
    override val entities:List[Entity] = List.tabulate(fftSize,fftLength/2)((x,y) => butterfly(x,y)).flatten
  }

  /** Parameterizable Butterfly Block */
  class ButterflyEntity(name:String,
                         val params:Butterfly.Params)(implicit clk:ClockControl) extends Entity.Leaf(name,name) {
    import params._

    val (add0, add1, angle) = FftMethods.getButterflyConstant(column, row, fftSize)


    val sig_in1  = complex(inputName  + "_"  + add0,INPUT, inputWidth)
    val sig_in2  = complex(inputName  + "_"  + add1,INPUT, inputWidth)
    val sig_out1 = complex(outputName + "_"  + add0,INPUT, outputWidth)
    val sig_out2 = complex(outputName + "_"  + add1,INPUT, outputWidth)

    override val signals = List(sig_in1,sig_in2,sig_out1,sig_out2)
    override def createModule = new Butterfly(this.name,this).createModule

  }
  /** Generic Class which handles the butterfly operation */
   class Butterfly(name:String,
     val entity:ButterflyEntity)(implicit clk:ClockControl) extends Module(name) {

     import entity._
     import entity.params._
     // Handling the Clock
     this.assignClock(clk)
     val n = clk
     // Block Parameters
     // Signal Creation
     /*val sig_in1  = complex(inputName + "_"  + add0,INPUT, inputWidth)
     val sig_in2  = complex(inputName + "_"  + add1,INPUT, inputWidth)
     val sig_out1 = complex(outputName + "_" + add0,INPUT, outputWidth)
     val sig_out2 = complex(outputName + "_" + add1,INPUT, outputWidth)
     */
     // Internal Signals
     val sig1R               = register(sig_in1)(2)
     val multiplier_out      = complex_reg("mult_out"  ,WIRE, internalWidth)(1)
     val adder_out1          = complex_reg("adder_out1",WIRE, internalWidth)(1)
     val adder_out2          = complex_reg("adder_out2",WIRE, internalWidth)(1)
     // Twiddle Factor Calculation
     val twiddle = complex(math.cos(math.Pi*angle.toDouble),math.sin(math.Pi*angle.toDouble), twiddleWidth)
     // Butterfly Body

     /- ("Twiddle Factor Multiplication")
     multiplier_out(n)   := RC(twiddle * sig_in1)
     /- ("Butterfly Adder")
     adder_out1(n)        := RC(multiplier_out(n-1) + sig_in2(n-2))
     adder_out2(n)        := RC(multiplier_out(n-1) - sig_in2(n-2))
     /- ("Signal Assignment")
     sig_out1             := adder_out1(n-1)
     sig_out2             := adder_out2(n-1)

   }

  object FftMain {
    class Params extends ModuleScope {
      val fftSize               = Parameter("Size",3)                -- "Size of the FFT (log 2)"
      val twiddleWidth          = Parameter("InternalWidth",S(8,6))  -- "Twiddle Factor Width"
      lazy val internalWidth    = Parameter("InternalWidth",List.tabulate(fftSize+1)(x => S(8,6))) -- "Internal Width"
      lazy val stageWidth       = Parameter("StageWidth",List.tabulate(fftSize+1)(x => S(8,6)))    -- "Internal Stage Output Width"

      lazy val stageParams =  List.tabulate(fftSize,math.pow(2.0,fftSize.toDouble).toInt/2)((x,y) => new Butterfly.Params(fftSize,
        stageWidth(x),
        stageWidth(x+1),
        internalWidth(x),
        twiddleWidth,
        x,
        y))
    }
  }

  object Butterfly {
    class Params(val fftSizeDefault:Parameter[Int],
                 val inputWidthDefault:Parameter[FixedType],
                 val outputWidthDefault:Parameter[FixedType],
                 val internalWidthDefault:Parameter[FixedType],
                 val twiddleWidthDefault:Parameter[FixedType],
                 val rowDefault:Parameter[Int],
                 val columnDefault:Parameter[Int]) {

      val fftSize       = Parameter("FFTSize",fftSizeDefault)             -- "Size of the FFT (log 2)"
      val inputName     = Parameter("InputName","signal_in")              -- "Input Signal Name"
      val outputName    = Parameter("OutputName","signal_out")            -- "Output Signal Name"
      val inputWidth    = Parameter("InputWidth",inputWidthDefault)       -- "Input Signal Width"
      val outputWidth   = Parameter("OutputWidth",outputWidthDefault)     -- "Output Signal Width"
      val internalWidth = Parameter("InternalWidth",internalWidthDefault) -- "Internal Signal Width"
      val twiddleWidth  = Parameter("TwiddleWidth",twiddleWidthDefault)   -- "Twiddle Signal Width"
      val row           = Parameter("Row",rowDefault)                     -- "Row"
      val column        = Parameter("Column",columnDefault)               -- "Column"
    }
  }


  def main(args:Array[String]) = {
    new FftParamTest().createProject2
  }




}

