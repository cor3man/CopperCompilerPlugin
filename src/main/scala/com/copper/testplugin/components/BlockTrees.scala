package com.copper.testplugin.components

import com.copper.testplugin.model.Struct

import scala.tools.nsc.Global
import scala.collection.mutable.ListBuffer

class BlockTrees[G <: Global](val global: G) {

  import global._

  var name: String = ""
  var classPackage: String = ""

  val listOfStructs: ListBuffer[Struct] = ListBuffer.empty

  var listOfRequests: ListBuffer[String] = ListBuffer.empty
  var listOfCommands: ListBuffer[String] = ListBuffer.empty
  var listOfEvents: ListBuffer[String] = ListBuffer.empty
  var listOfSubscribes: ListBuffer[String] = ListBuffer.empty

  val guard: Tree => Unit = BlockTreesTraverser.traverse

  def getClassFileStruct(tree: Tree): ListBuffer[Struct] = {
    listOfRequests.clear()
    listOfSubscribes.clear()
    listOfCommands.clear()
    listOfEvents.clear()
    listOfStructs.clear()

/*    def t = tree match {
      case t @ PackageDef(Select(a, b), _) => {
        //printWithTabs()
        classPackage = (a + "." + b)
        printWithTabs("PackageDef : " + classPackage, 1)
        t
      }
    }*/

    BlockTreesTraverser.traverse(tree)
    println("structs:")
    listOfStructs.foreach(l => println(l.toJsonString))
    listOfStructs
  }

  private object BlockTreesTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
      //printWithTabs("Start travers")
      //println(showRaw(tree))
      //name = tree.pos.source.file.name
      tree match {
        case t @ PackageDef(Select(a,b), _) => {
          //printWithTabs()
          classPackage = (a + "." + b)
          printWithTabs("PackageDef : "+ classPackage, 1)
        }
        case t @ ClassDef(_, typeName, _, _) => {
          //
          printWithTabs("------------ClassDef begin -------------", 1)
          printWithTabs("++++++++++++++ " + showRaw(t))
          val className = classPackage + "." + typeName
          printWithTabs(classPackage + "." + typeName, 2)
          printWithTabs(showRaw(tree), 2)

          tree.children.foreach(t => printWithTabs("children  : "+ showRaw(t), 4))
          //tree.children.foreach(ClassTraverser.traverse)
          ClassTraverser.traverse(t)
          def subStruct = Struct(className, listOfRequests, listOfSubscribes, listOfCommands, listOfEvents)
          printWithTabs(subStruct.toJsonString, 3)

          listOfStructs.append(subStruct)

          listOfSubscribes = ListBuffer.empty
          listOfRequests = ListBuffer.empty
          listOfCommands = ListBuffer.empty
          listOfEvents = ListBuffer.empty

          printWithTabs("------------ClassDef end -------------", 1)
        }
        case ModuleDef(_, typeName, _) => {
          printWithTabs("------------S - ModuleDef begin -------------", 1)

          val objectName = classPackage + "." + typeName
          printWithTabs(classPackage + "." + typeName, 2)
          printWithTabs(showRaw(tree), 2)
          //println(t)
          tree.children.foreach(ClassTraverser.traverse)
          def subStruct = Struct(objectName, listOfRequests, listOfSubscribes, listOfCommands, listOfEvents)

          listOfStructs.append(subStruct)

          listOfSubscribes = ListBuffer.empty
          listOfRequests = ListBuffer.empty
          listOfCommands = ListBuffer.empty
          listOfEvents = ListBuffer.empty

          printWithTabs("------------E - ModuleDef end -------------", 1)
        }
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private object ClassTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
      name = tree.pos.source.file.name
      tree match {
        case Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(_*)), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(tree), 3)
          listOfRequests.append(value.toString)
        }
        case Apply(Select(Ident(TermName("sbus")), TermName("command")), List(Literal(Constant(value)), _*)) => {
          printWithTabs(showRaw(tree), 3)
          listOfCommands.append(value.toString)
        }
        case Apply(Select(Ident(TermName("sbus")), TermName("event")), List(Literal(Constant(value)), _*)) => {
          printWithTabs(showRaw(tree), 3)
          listOfEvents.append(value.toString)
        }
        case Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("on")), List(_, _)), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(tree), 3)
          listOfSubscribes.append(value.toString)
        }
        case Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
          printWithTabs(showRaw(tree), 3)
          listOfSubscribes.append(value.toString)
        }
        case _ =>
      }
      super.traverse(tree)
    }

  }

  def printWithTabs(s: String, tabs: Int = 0): Unit = {
    //(1 to tabs).foreach(_ => print("-" + _))
    println(("  " * tabs) + s)
  }
}


