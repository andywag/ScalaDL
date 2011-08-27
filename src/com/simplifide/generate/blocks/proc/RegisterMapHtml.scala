package com.simplifide.generate.blocks.proc

import com.simplifide.generate.language.ExtraFile
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.parser.block.state.State
import com.simplifide.generate.html.{Description, HtmlUtilities, HtmlTable}
import com.simplifide.generate.language.Conversions._
/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/22/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 */

class RegisterMapHtml(override val filename:String,val registerMap:RegisterMap) extends ExtraFile {


     val stateContents = {
       def row(addressItem:Address):List[List[Description]] = {
         def item(address:Int,item:Address.Item):List[Description] = {
           List(address.toString,
             item.register.name,
             if (item.register.width == 0) item.location.toString else "[" + (item.location + item.register.width-1) + ":" + item.location + "]",
             item.register.typeString,
             item.register.default.toString,
             item.register.description.getOrElse("")
           )
         }
         addressItem.registers.map(x => (addressItem.address,x)).map(x => item(x._1,x._2))
       }

       val head:List[Description] = List("Address","Name","Location","Type","Default","Description")
       val body:List[List[Description]] = registerMap.sortedAddresses.map(x => row(x._2)).reduceLeft(_ ::: _)
       val htmlModel = new HtmlTable(head,body,Some("Register Map Table"))
       htmlModel.createTable
    }


    val contents = HtmlUtilities.fullHtml(filename + " State Machine State Description",List(stateContents)).toString()

}