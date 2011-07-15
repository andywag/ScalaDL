package com.simplifide.generate.blocks.basic.fixed

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.signal.FixedType
import com.simplifide.generate.generator.SimpleSegment
import java.lang.Boolean
import com.simplifide.generate.blocks.basic.SimpleStatement

/** Convenience Methods to create Adders */
object AdditionStatement {

  def RoundClip(name:String, out:SimpleSegment,seg1:SimpleSegment, seg2:SimpleSegment,
                neg1:Boolean = false, neg2:Boolean = false,
                internal:Option[FixedType] = None):SimpleSegment = {
    val first  = createFirstTerm(seg1,neg1)
    val second = createSecondTerm(seg2,neg2)

    val segment = new AdditionSegment.RoundClip(name,List(first,second),out.fixed,internal)
    new SimpleStatement.Assign(out,segment)
  }

  private def createFirstTerm(segment:SimpleSegment, neg:Boolean):AdditionTerm =
    if (neg) new AdditionTerm.SubTerm(segment) else new AdditionTerm.Empty(segment)



  private def createSecondTerm(segment:SimpleSegment, neg:Boolean):AdditionTerm =
    if (neg) new AdditionTerm.SubTerm(segment) else new AdditionTerm.AddTerm(segment)


  private def createTerms(segment1:SimpleSegment,segment2:SimpleSegment,neg1:Boolean,neg2:Boolean):List[AdditionTerm] = {
    return List(createFirstTerm(segment1,neg1),createSecondTerm(segment2,neg2))
  }


}
