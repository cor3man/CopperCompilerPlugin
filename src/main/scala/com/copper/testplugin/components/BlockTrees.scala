package com.copper.testplugin.components

import com.copper.testplugin.model.Struct

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.collection.mutable.ListBuffer

class BlockTrees[G <: Global](val global: G) {

  import global._

  val listOfStructs: ListBuffer[Struct] = ListBuffer.empty

  val guard: Tree => Unit = BlockTreesTraverser.traverse

  def getClassFileStruct(tree: Tree): ListBuffer[Struct] = {
    BlockTreesTraverser.traverse(tree)

    println("structs:")
    listOfStructs.foreach(l => println(l.toJsonString))
    listOfStructs
  }

  private object BlockTreesTraverser extends Traverser {
    var names = mutable.Stack[String]()

    def processBody(trees: List[Tree]): Unit = {
      printWithTabs("testMethod start", 2)
      trees.foreach(t => printWithTabs("List: " + showRaw(t), 2))
      trees.foreach {
        case t @ ClassDef(_, typeName, _, Template(_, _, list)) => {
          names.push(typeName.toString()+ "$")
          processBody(list)
        }
        case t @ ModuleDef(_, typeName, Template(_, _, list)) => {
          names.push(typeName.toString()+ "$")
          processBody(list)
        }
        case _ =>
      }
      listOfStructs.append(StructBuilder.build(trees, names.reverse.mkString))
      names.pop()
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case t @ PackageDef(Select(a, b), list) => {
          printWithTabs("PackageDef: " + showRaw(t), 1)
          list.foreach(t => printWithTabs(showRaw(t)))
          val classPackage = (a + "." + b + ".")
          names.push(classPackage)
          printWithTabs("PackageDef : " + classPackage, 1)
          processBody(list)
        }
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private object StructBuilder {
    def build(list: List[Tree], name: String): Struct = {
      printWithTabs("------------build start -------------", 1)

      var listOfRequests: ListBuffer[String] = ListBuffer.empty
      var listOfCommands: ListBuffer[String] = ListBuffer.empty
      var listOfEvents: ListBuffer[String] = ListBuffer.empty
      var listOfSubscribes: ListBuffer[String] = ListBuffer.empty

      list.foreach {
        case t @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(_*)), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(t), 3)
          listOfRequests.append(value.toString)
        }
        case t @ Apply(Select(Ident(TermName("sbus")), TermName("command")), List(Literal(Constant(value)), _*)) => {
          printWithTabs(showRaw(t), 3)
          listOfCommands.append(value.toString)
        }
        case t @ Apply(Select(Ident(TermName("sbus")), TermName("event")), List(Literal(Constant(value)), _*)) => {
          printWithTabs(showRaw(t), 3)
          listOfEvents.append(value.toString)
        }
        case t @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("on")), List(_, _)), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(t), 3)
          listOfSubscribes.append(value.toString)
        }
        case t @ Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(t), 3)
          listOfSubscribes.append(value.toString)
        }
        case t @ DefDef(Modifiers(_, _, listOfMods), _, _, _, _, _) => {
          printWithTabs(showRaw(t), 3)
          listOfMods.foreach {
            case Apply(Select(New(Ident(TypeName("Subscribe"))), _), List(Literal(Constant(value)))) => listOfSubscribes.append(value.toString)
            case _ =>
          }
        }
        case _ =>
      }

      val struct = Struct(name, listOfRequests, listOfSubscribes, listOfCommands, listOfEvents)
      printWithTabs("Struct : " + struct, 2)
      printWithTabs("------------build end -------------", 1)
      struct
    }
  }

  def printWithTabs(s: String, tabs: Int = 0): Unit = {
    println(("  " * tabs) + s)
  }
}


