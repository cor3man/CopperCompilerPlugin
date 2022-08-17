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
    listOfStructs
  }

  private object BlockTreesTraverser extends Traverser {
    var names = mutable.Stack[String]()

    def processBody(trees: List[Tree]): Unit = {
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
      val name = names.reverse.mkString
      val struct = Struct(name.substring(0, name.length-1), ListBuffer.empty, ListBuffer.empty, ListBuffer.empty, ListBuffer.empty)
      listOfStructs.append(StructBuilder.build(trees,struct))
      names.pop()
    }

    override def traverse(tree: Tree): Unit = {
      //println(showRaw(tree))
      tree match {
        case t @ PackageDef(Select(a, b), list) => {
          val classPackage = (a + "." + b + ".")
          names.push(classPackage)
          processBody(list)
        }
        case t @ PackageDef(a, list) => {
          val classPackage = (a + ".")
          names.push(classPackage)
          processBody(list)
        }
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private object StructBuilder {
    def build(list: List[Tree], struct: Struct): Struct = {
      println("----------------Start building ------------------")
      //list.foreach(l=> println(showRaw(l)))
      list.foreach {
        case t @DefDef(Modifiers(_, _, listOfMods), _, _, _, _, apply) => {
          listOfMods.foreach {
            case Apply(Select(New(Ident(TypeName("Subscribe"))), _), List(Literal(Constant(value)))) => struct.subscribes.append(value.toString)
            case _ =>
          }

          build(apply.filter {
            case t @ Apply(TypeApply(Select(Ident(TermName("sbus")), _), List(_*)), List(_, _*)) => true
            case _ => false
          }, struct)
        }
        case t @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("request")), List(_*)), List(Literal(Constant(value)), _*)) => {
          struct.requests.append(value.toString)
        }
        case t @ Apply(Select(Ident(TermName("sbus")), TermName("command")), List(Literal(Constant(value)), _*)) => {
          struct.commands.append(value.toString)
        }
        case t @ Apply(Select(Ident(TermName("sbus")), TermName("event")), List(Literal(Constant(value)), _*)) => {
          struct.events.append(value.toString)
        }
        case t @ Apply(TypeApply(Select(Ident(TermName("sbus")), TermName("on")), List(_, _)), List(Literal(Constant(value)))) => {
          struct.subscribes.append(value.toString)
        }
        case t @ Apply(Select(New(Ident(TypeName("Subscribe"))), termNames.CONSTRUCTOR), List(Literal(Constant(value)))) => {
          struct.subscribes.append(value.toString)
        }
        case _ =>
      }
      struct
    }
  }

  def printWithTabs(s: String, tabs: Int = 0): Unit = {
    println(("  " * tabs) + s)
  }
}


