package com.copper.testplugin

import com.copper.testplugin.components.BlockTrees
import com.copper.testplugin.model.Struct

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class CCPlugin(val global: Global) extends Plugin {

  override val name: String = "ccplugin"
  override val description: String = "Copper compiler plugin"
  override val components: List[PluginComponent] = List(new MyComponent(global))

  override def init(options: List[String], error: String => Unit): Boolean = {
    myOptions = new Properties()
    options foreach { str =>
      str.indexOf('=') match {
        case -1 => myOptions.setProperty(str, "true")
        case n => myOptions.setProperty(str.substring(0, n), str.substring(n + 1))
      }
    }
    true
  }

  private var myOptions: Properties = _

  private class MyComponent(val global: Global) extends PluginComponent {

    import global._

    override val phaseName: String = "ccplugin"
    override def description: String = "collect sbus invocations"
    override val runsAfter: List[String] = List("parser")
    //override val runsAfter: List[String] = List("typer")

    private val guardian = new BlockTrees[global.type](global)

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("\n********** Phase start ***********")
        val struct = guardian.getClassFileStruct(unit.body)
        println("\n Classes to save ----------------- >  : ")
        struct.foreach(s => println(s.name))

        struct.foreach(s => {
          if (!s.isEmpty) writeFile(myOptions.get("out").toString, myOptions.get("prj") + "-" + s.name + ".json", s)
        })

        println("\n********** Phase end ***********")
      }
    }

    def writeFile(dirName: String, filename: String, struct: Struct): Unit = {
      val dir: File = new File(dirName)
      dir.mkdirs()
      val file = new File(dirName + filename)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(struct.toJsonString)
      bw.close()
    }
  }
}
