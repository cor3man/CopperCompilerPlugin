package com.copper.testplugin


import com.copper.testplugin.components.GuardedBlocksComponent

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class GuardedBlocksPlugin(val global: Global) extends Plugin {

  override val name: String = "guardedblocks"

  override val description: String = "GuardedBlocks compiler plugin"

  override val components: List[PluginComponent] = List(new GuardedBlocksComponent(global))
}
