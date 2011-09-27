package com.simplifide.generate.html

import xml.Node

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/22/11
 * Time: 9:16 PM
 * To change this template use File | Settings | File Templates.
 */

object HtmlUtilities {

  def fullHtml(title:String,internal:List[Node]):Node = {
     <html>
      <head>
        <title>{title}</title>
      </head>
      <body>
        {internal}
      </body>
    </html>
  }

}