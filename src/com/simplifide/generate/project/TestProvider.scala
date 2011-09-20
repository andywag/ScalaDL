package com.simplifide.generate.project

import com.simplifide.generate.util.FileOps
import collection.mutable.{ListBuffer, LinkedHashMap}
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/1/11
 * Time: 4:08 PM
 * To change this template use File | Settings | File Templates.
 */

trait TestProvider {  /*
  val name1:String
  /** Default Simulation Length */
  val simLength:Int = 1000;
  /** Enable or Disables Tracing for Simuation */
  val trace:Boolean = false
    /** Defines a set of default conditions for testing a module */
  def getStimulusMap:Stimulus.Map = new Stimulus.Map(new LinkedHashMap[SignalTrait,Stimulus](),this)
  /** Defines the set of variables to dump */
  def getStorageMap:Storage.Map   = new Storage.Map(new LinkedHashMap[SignalTrait,Storage]())

  private def createTestbench(src:String, segment:ModuleSegment):Impl = {
    val tb = new TestBench(name1 + "_test",segment)
    new Impl.Segment(name1 + "_test",src,tb)
  }

  def createTestCode(location:TestProvider.Locations, segment:ModuleSegment, writer:CodeWriter) {
	  if (segment != null) {
		  FileOps.createFile(location.src,getTestName    + ".cpp",createCpp(segment))
		  FileOps.createFile(location.test,getTestName   + ".mk", createMakefile(segment))
		  FileOps.createFile(location.matlab,getTestName + ".m",  segment.getStorageMap.getMatlabLoadCommands)
		  writer.createCode(createTestbench(location.src,segment))
	  }
  }


  private def prependV:String = "V" + getTestName
  private def getTestName:String = name1 + "_test"

  protected def createCpp(moduleSegment:ModuleSegment):String  = {
    val builder = new StringBuilder
    if (this.trace) {
        builder.append("#include")
        builder.append("\"verilated_vcd_c.h\"\n")
    }

    builder.append("#include <verilated.h>\n")
    builder.append("#include \""); builder.append(this.prependV); builder.append(".h\"\n\n")
    builder.append("unsigned int main_time = 0;\n\n")
    builder.append("double sc_time_stamp () {\n")
    builder.append("    return main_time;\n")
    builder.append("}\n\n\n")
    builder.append("\n\n")
    builder.append(prependV)
    builder.append(" *top;\n\n")
    builder.append("int main(int argc, char** argv) {\n")
    builder.append("    Verilated::commandArgs(argc, argv);\n")
    builder.append("    top = new "); builder.append(prependV); builder.append(";\n")
    if (this.trace) {
        builder.append("Verilated::traceEverOn(true);\n")
        builder.append("VerilatedVcdC* tfp = new VerilatedVcdC;\n")
        builder.append("top->trace (tfp, 99);\n")
        builder.append("tfp->open (\"simx.vcd\");\n")
    }
    builder.append("    while (!Verilated::gotFinish()) {\n")
    builder.append("       if ((main_time % 4) == 2) {\n")
    builder.append("              top->clk = 1;\n")
    builder.append("       }")
    builder.append("       else if ((main_time % 4) == 0) {\n")
    if (this.trace) {builder.append("              tfp->dump(main_time);\n") }
    builder.append("              top->clk = 0;\n")
    builder.append("       } ")
    builder.append("       top->eval();\n")
    builder.append("       main_time++;\n")
    builder.append("    }\n")
    builder.append(" top->final();\n\n")
    if (this.trace) {
      builder.append("tfp->close();\n")
    }
    builder.append("}\n\n")
    builder.toString
    //FileOps.createFile(getTestSourceLocation(location),getTestName + ".cpp",builder.toString)
  }

  protected def createMakefile(moduleSegment:ModuleSegment):String =  {
      val builder = new StringBuilder
      builder.append("DESIGN    = ../design/gen\n")
      // ProjectStructure Location Handling
      for (project <- moduleSegment.projects) {
        builder.append(project.name1.capitalize); builder.append("LOCATION = ")
        builder.append(project.location); builder.append("/design/gen\n")
      }
      builder.append("ARGS      = -cc -Wno-COMBDLY")
      if (moduleSegment.trace) builder.append(" --exe --trace ")
      builder.append("\n\n\n")
      builder.append("SOURCE    = ")
      val files = new ListBuffer[String]
      files.append("src/" + getTestName)
      files.appendAll(moduleSegment.getFileList.map(x => "${DESIGN}/" + x))
      //files.append("${DESIGN}/" + moduleSegment.name1 )
      for (project <- moduleSegment.projects) {
        val na = "${" + project.name1.capitalize + "LOCATION}/"
        project.moduleSegment.getFileList.foreach(x => files.append(na + x))
      }

      var first = true
      for (file <- files) {
        if (!first) builder.append(" \\\n        ")
        builder.append(file + ".v")
        first = false
      }
      builder.append("\n\n")
      builder.append("CSOURCE   = src/")
      builder.append(getTestName)
      builder.append(".cpp\n\n")

      builder.append("run :\n")
	    builder.append("\tverilator ${ARGS} ${SOURCE} -exe ${CSOURCE};\n")
	    builder.append("\tmake -C obj_dir -j -f ");
      builder.append(prependV)
      builder.append(".mk ")
      builder.append(prependV)
      builder.append(";\n")
	    builder.append("\tcp obj_dir/")
      builder.append(prependV)
	//complex2vector.sh data/xr_in.txt data/xr_in.bin 8 8;
	    builder.append(" .;\n")
      builder.append("\t./")
      builder.append(prependV)
      builder.toString

	//vector2complex.sh data/data_out.bin data/data_out.txt 8 8;
	//vector2complex.sh data/data_in.bin data/data_in.txt 8 8;

      //FileOps.createFile(getTestLocation(location),getTestName + ".mk",builder.toString)
  }
        */

}

object TestProvider {
  class Locations(val test:String, val src:String, val matlab:String)
}