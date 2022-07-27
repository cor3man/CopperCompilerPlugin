package com.copper.testplugin.components

import com.copper.testplugin.model.Struct

import scala.tools.nsc.Global
import scala.collection.mutable.ListBuffer

class BlockTreesGuardian[G <: Global](val global: G) {

  import global._
  var name: String = ""
  val listOfRequests: ListBuffer[String] = ListBuffer("")
  val listOfSubscribes: ListBuffer[String] = ListBuffer("")

  val guard: Tree => Unit = BlockTreesGuardianTraverser.traverse

  def getStruct(tree: Tree): Struct = {
    listOfRequests.clear()
    listOfSubscribes.clear()
    BlockTreesGuardianTraverser.traverse(tree)
    Struct(name, listOfRequests, listOfSubscribes)
  }

  private object BlockTreesGuardianTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
        //println(showRaw(tree))
        name = tree.pos.source.file.name
        tree match {
          case Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(Ident(TypeName(_)))), List(Literal(Constant(value)))) => {
            listOfRequests.append(s"sbus.request($value)")
          }
          case Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
            listOfSubscribes.append(s"@Subscribe($value)")
          }
          case _ =>
        }
      super.traverse(tree)
    }
  }
}


