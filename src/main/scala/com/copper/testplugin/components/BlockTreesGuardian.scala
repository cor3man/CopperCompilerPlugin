package com.copper.testplugin.components

import scala.tools.nsc.Global

class BlockTreesGuardian[G <: Global](val global: G) {

  import global._
  var listOfInvocations: List[String] = List("")
  val guard: Tree => Unit = BlockTreesGuardianTraverser.traverse


  def find(tree: Tree): List[String] = {
    BlockTreesGuardianTraverser.traverse(tree)
    listOfInvocations
  }

  private object BlockTreesGuardianTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
        //println(show(tree))
        //println(showRaw(tree))
        tree match {
          case block @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(Ident(TypeName("Unit")))), List(Literal(Constant(value)))) => {
            //println("------------------------------------------")
            //println(showRaw(block))
            //println(s"sbus.request($value)")
            //think about it!!! ---- listOfInvocations ++ (s"sbus.request($value)")
            listOfInvocations = (tree.pos.source.path + " : " + s"sbus.request($value)") :: listOfInvocations
            //println("------------ " + tree.pos.source.file)
            //println("------------------------------------------")
          }
          case block @ Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
            //println("------------------------------------------")
            //println(showRaw(block))
            //println(s"@Subscribe($value)")
            listOfInvocations = (tree.pos.source.path + " : " + s"@Subscribe($value)") :: listOfInvocations
            //println(tree.pos.source.path)
            //println("------------------------------------------")
          }
          case _ =>
        }
      super.traverse(tree)
    }
  }
}


