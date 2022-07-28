package com.copper.testplugin.components

import com.copper.testplugin.model.Struct

import scala.tools.nsc.Global
import scala.collection.mutable.ListBuffer

class BlockTrees[G <: Global](val global: G) {

  import global._
  var name: String = ""
  val listOfRequests: ListBuffer[String] = ListBuffer("")
  val listOfCommands: ListBuffer[String] = ListBuffer("")
  val listOfEvents: ListBuffer[String] = ListBuffer("")
  val listOfSubscribes: ListBuffer[String] = ListBuffer("")

  val guard: Tree => Unit = BlockTreesTraverser.traverse

  def getStruct(tree: Tree): Struct = {
    listOfRequests.clear()
    listOfSubscribes.clear()
    listOfCommands.clear()
    listOfEvents.clear()
    BlockTreesTraverser.traverse(tree)
    Struct(name, listOfRequests, listOfSubscribes, listOfCommands, listOfEvents)
  }

  private object BlockTreesTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
        //println(showRaw(tree))
        name = tree.pos.source.file.name
        tree match {
/*          case t @ PackageDef(_,_) => {
            println("--------------------------")
            println(t.summaryString)
            println("--------------------------")

          }*/
          case Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(_)), List(Literal(Constant(value)), _*)) => {
            listOfRequests.append(value.toString)
          }
          case Apply(Select(Ident(TermName("sbus")), TermName("command")), List(Literal(Constant(value)), _*)) => {
            listOfCommands.append(value.toString)
          }
          case Apply(Select(Ident(TermName("sbus")), TermName("event")), List(Literal(Constant(value)), _*)) => {
            listOfEvents.append(value.toString)
          }
          case Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("on")), List(_, _)), List(Literal(Constant(value)))) => {
            listOfSubscribes.append(value.toString)
          }
          case Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
            listOfSubscribes.append(value.toString)
          }
          case _ =>
        }
      super.traverse(tree)
    }
  }
}


