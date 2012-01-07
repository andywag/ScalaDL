package com.simplifide.generate.blocks.proc2

import com.simplifide.generate.blocks.proc.Address

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/3/12
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */

class RegisterMapNew(val groups:List[RegisterGroup]) {

  val addresses = groups.flatMap(x => x.addresses).sortBy(x => x.address)

}

object RegisterMapNew {

}