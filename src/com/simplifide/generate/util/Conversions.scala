package com.simplifide.generate.util

import com.simplifide.generate.signal.{FixedType, SignalTrait}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/11/11
 * Time: 11:18 AM
 * To change this template use File | Settings | File Templates.
 */

class Conversions {

}

object Conversions {
  implicit def SignalTrait2Fixed(signal:SignalTrait):FixedType = signal.fixed

}