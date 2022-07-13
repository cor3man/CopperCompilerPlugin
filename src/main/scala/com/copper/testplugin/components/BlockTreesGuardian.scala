package com.copper.testplugin.components

import scala.tools.nsc.Global

class BlockTreesGuardian[G <: Global](val global: G) {

  import global._

  val guard: Tree => Unit = BlockTreesGuardianTraverser.traverse

  private object BlockTreesGuardianTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
        //println(show(tree))
        //println(showRaw(tree))
        tree match {
          case block @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(Ident(TypeName("Unit")))), List(Literal(Constant(value)))) => {
            println("------------------------------------------")
            println(showRaw(block))
            println(s"sbus.request($value)")
            println(tree.pos)
            println("------------------------------------------")
          }
/*
          case block @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(Ident(TypeName("Unit")))), List(Ident(TermName("str")))) => {
            println("------------------------------------------")
            println(showRaw(block))
            println("sbus.request " + value)
            println("------------------------------------------")
          }
*/
          case block @ Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
            println("------------------------------------------")
            println(showRaw(block))
            println(s"@Subscribe($value)")
            println(tree.pos)
            println("------------------------------------------")
          }
          case _ =>
        }
      super.traverse(tree)
    }
  }
}


