package com.copper.testplugin.model

import scala.collection.mutable.ListBuffer

case class Struct(name: String,
                  requests: ListBuffer[String],
                  subscribes: ListBuffer[String],
                  commands: ListBuffer[String],
                  events: ListBuffer[String]) {
  def isEmpty: Boolean = {
    requests.isEmpty && subscribes.isEmpty && commands.isEmpty && events.isEmpty
  }
  def toJsonString: String = {
    def listFormatter(list: ListBuffer[String]): String = {
      list.map(s => "\"" + s + "\"").mkString("[", ",", "]")
    }

    val formattedRequests = listFormatter(requests)
    val formattedSubscribes = listFormatter(subscribes)
    val formattedCommands = listFormatter(commands)
    val formattedEvents = listFormatter(events)

    "{\"ClassName\":\"" + name + "\",\"request\":"+ formattedRequests + ",\"subscribe\":" + formattedSubscribes + ",\"command\":" + formattedCommands + ",\"event\":" + formattedEvents + "}"
  }
}

