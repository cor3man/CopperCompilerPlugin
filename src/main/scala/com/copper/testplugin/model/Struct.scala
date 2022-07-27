package com.copper.testplugin.model

import scala.collection.mutable.ListBuffer

case class Struct(val name: String, requests: ListBuffer[String], subscribes: ListBuffer[String]) {
  def toJsonString: String = {
    val formatedRequests = requests.map(s => "\"" + s + "\"").mkString("[", ",", "]")
    val formatedSubscribes = subscribes.map(s => "\"" + s + "\"").mkString("[", ",", "]")
    "{\"ClassName\":\"" + name + "\",\"request\":"+ formatedRequests + ",\"subscribe\":" + formatedSubscribes + "}"
  }
}
