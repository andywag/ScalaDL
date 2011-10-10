package com.simplifide.generate.language

import com.simplifide.generate.html.Description
import com.simplifide.generate.parser.model.Signal
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/26/11
 * Time: 1:35 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Trait which holds an html or string description. This was a bit of an afterthought and uses a mutable value
 * This should be refactored in the future
 */
trait DescriptionHolder {

  var description:Option[Description] = None

  def -- (description:Description)  = {
    this.description = Some(description)
    this
  }
}