package com.simplifide.generate.html

import xml.Attribute._
import xml.Text._
import xml._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/22/11
 * Time: 4:02 PM
 * To change this template use File | Settings | File Templates.
 */

class HtmlTable(val head:List[Description],val body:List[List[Description]],val caption:Option[String] = None) {

  private def createHead(tit:Description) =
    <th><strong>{tit.html}</strong></th>  % Attribute(None, "BGCOLOR", Text("#CCCCFF"), Null)

  private def createElement(str:Description) = <td>{str.html}</td>


  private def createBody(row:List[Description]) = {
    <tr>{row.map(x => createElement(x))}</tr>
  }


  def createTable = {
    val tab =
      <table>
        {if (caption != None) <caption>{caption.get}</caption>}
          <tr>
            {head.map(x => createHead(x))}
          </tr>
        {body.map(x => createBody(x))}
       </table>

    tab % Attribute(None, "border", Text("1"), Null)
  }

  def htmlTable = {
     <html>
      <head>
      </head>
      <body>
        {createTable}
      </body>
    </html>
  }

}