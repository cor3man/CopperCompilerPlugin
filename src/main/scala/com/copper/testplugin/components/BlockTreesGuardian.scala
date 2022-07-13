package com.copper.testplugin.components

import scala.tools.nsc.Global

class BlockTreesGuardian[G <: Global](val global: G) {

  import global._

  val guard: Tree => Unit = BlockTreesGuardianTraverser.traverse

  private object BlockTreesGuardianTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
        //println(show(tree))
        println(showRaw(tree))
        tree match {
/*        case block @ Function(vparam, body) => {println(vparam)}
          case block @ If(cond, thenp, elsep) => {println(s"if block: $cond : $thenp : $elsep")}
          case block @ TypeDef(mods, name, tparams, rhs) => {println(s"TypeDef block: $mods : $name : $tparams")}
          case block @ ValDef(mods, name, tpt, rhs) => {println(s"ValDef block: $mods : $name : $tpt : $rhs")}*/
          //case block @ Apply(fun, args) => {println(s"Apply block: $fun : $args")}
          //case block @ Function(vparam, body) => {println(vparam)}
          case block @ Apply(fun, args) if fun.toString().equals("sbus.request") => {
            println("------------------------------------------")
            println(s"Apply block: $fun : $args")
            println("Show args rows: " + showRaw(args))
            args.foreach {
              case Literal(value) => {
                println(s"Literal block: $value")
              }
              case _ =>
            }
            println("------------------------------------------")
          }
          //case block @ TypeApply(fun, args) => println(s"TypeApply block: $fun. : $args")
          //case block @ Ident(vparam) => {println(vparam) }
          //case block @ Literal(value) => {println(s"Literal block: $value")}
          //case block @ ApplyDynamic(qual, args) => {println(s"ApplyDynamic block: $qual : $args")}
          //case block @ Select(qualifier, name) => {println(s"Select block: $qualifier : $name")}
          case _ =>
        }
      super.traverse(tree)
    }
  }
}


