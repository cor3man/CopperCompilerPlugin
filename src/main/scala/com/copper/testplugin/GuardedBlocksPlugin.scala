package com.copper.testplugin

import com.copper.testplugin.components.BlockTreesGuardian
import com.copper.testplugin.model.Struct
import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class GuardedBlocksPlugin(val global: Global) extends Plugin {

  override val name: String = "guardedblocks"
  override val description: String = "GuardedBlocks compiler plugin"
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

    override val phaseName: String = "guardblocks"
    override def description: String = "check for unused statements in code blocks"
    override val runsAfter: List[String] = List("parser") //change a phase

    private val guardian = new BlockTreesGuardian[global.type](global)

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("********** Phase start ***********")
        val struct = guardian.getStruct(unit.body)
        println(struct.toJsonString)
        writeFile(myOptions.get("out") + struct.name + ".json", struct)
        println("********** Phase end ***********")
      }
    }

    def writeFile(filename: String, struct: Struct): Unit = {
      val file = new File(filename)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(struct.toJsonString)
      bw.close()
    }
  }
}
