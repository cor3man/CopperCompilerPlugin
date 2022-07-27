package com.copper.testplugin.components

import java.io.{BufferedWriter, File, FileWriter}
import scala.tools.nsc._
import scala.tools.nsc.classpath.{AggregateClassPath, DirectoryClassPath}
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.util.ClassPath

class GuardedBlocksComponent(val global: Global) extends PluginComponent {

  import global._

  override val phaseName: String = "guardblocks"
  override def description: String = "check for unused statements in code blocks"
  override val runsAfter: List[String] = List("parser") //change a phase

  private val guardian = new BlockTreesGuardian[global.type](global)

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      //guardian.guard(unit.body)
      println(classPath.asURLs.last)

      def path = classPath.asURLs.last

/*      classPath match {
        //case AggregateClassPath(Seq(dcp: ClassPath)) => println("???!!!" )
        case AggregateClassPath(aggregates) => {
          aggregates match {
            case s: Vector[DirectoryClassPath] => println(s.last.)
          }
        }
      }*/
      println("********************* Phase start")
      //val l = guardian.find(unit.body)
      val l = guardian.find(unit.body)
      l.foreach(println(_))
      writeFile(path.getPath + "..//..//..//ListOfInvocations.log", l)
      println("********************* Phase end")
    }
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line + "\n")
    }
    bw.close()
  }
}