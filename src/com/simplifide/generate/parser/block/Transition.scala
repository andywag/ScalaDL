package com.simplifide.generate.parser.block

//import scalax.collection.GraphEdge.{DiEdge, ExtendedKey, EdgeCopy}
//import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 5/13/11
 * Time: 12:46 PM
 * To change this template use File | Settings | File Templates.
 */

/*
class Transition[State](nodes: Product,
                        val expression:Expression = null)
    extends DiEdge[State](nodes)
    with    ExtendedKey[State]
    with    EdgeCopy[Transition]
  {


    def headModule:State = nodes.productElement(0).value.asInstanceOf[State]
    def tailModule:State = nodes.productElement(1).value.asInstanceOf[State]


    def attributes = List(expression)
    override def keyAttributes = List(expression)
    override protected def attributesToString = " (" + expression + ")"

    override def copy[NN](newNodes: Product) = {
      new Transition[NN](newNodes,expression)
    }
    def copyWithEdgeIn[NN](newNodes: Product):Transition.TransitionIn[NN] = {
      new Transition[NN](newNodes,expression) with EdgeIn[NN,Transition.TransitionIn]
    }


  }

  object Transition{
    def apply(from: State,
              to: State,
              expression:Expression) = new Transition[State](NodeProduct(from, to),expression)



    def unapply(e: Transition[State]) = Some(e)

    type TransitionIn[A] = Transition[A] with EdgeIn[A,Transition]
    type TransitionState = TransitionIn[State]

    @inline implicit def TransitionToEdgeIn[A <: State](e: Transition[A]) =
      new Transition[A](e.nodes,e.expression) with EdgeIn[A,TransitionIn]

    final class InstanceAssoc[A <: State](val e: DiEdge[A]) {
    @inline final def ## (expression:Expression) =
        new Transition[A](e.nodes, expression) with EdgeIn[A,TransitionIn]

    }
    @inline final implicit def edge2InstanceAssoc[A <: State](e: DiEdge[A])
      = new InstanceAssoc[A](e)
  }
  */