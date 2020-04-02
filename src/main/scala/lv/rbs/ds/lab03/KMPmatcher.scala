package main.scala.lv.rbs.ds.lab03

import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class KMPmatcher(var pattern: String) {

  var comparasions:Int = 0
  var steps:List[Map[String, String]] = List()

  // Code taken from https://codereview.stackexchange.com/questions/212068/kmp-algorithm-in-scala

  // Add some initialization steps -> you can compute the prefix function...
  // loops...
  def getPrefixFun(): ArrayBuffer[Int] = {

    val lookupTable = ArrayBuffer.fill(pattern.length + 1)(-1)
    lookupTable(0) = -1 // first char always 0

    var len = 0
    var i = 1

    while( i < pattern.length) {
      if (pattern(i) == pattern(len)) {
        len += 1
        lookupTable(i) = len
        i += 1
      }
      else {
        // mismatch
        if (len == 0) {
          lookupTable(i) = 0;
          i= i + 1
        }
        else {
          len = lookupTable(len-1)
        }
      }
    }
    lookupTable
  }

  def findAllIn(t: CharSequence): Iterator[Int] = {
    var startCounter:Int = 0

    var result: ArrayBuffer[Int] = ArrayBuffer()
    if (pattern.length > t.length) println("bad input")
    else if (pattern == t) println("same strings")
    else {
      val lookupTable = getPrefixFun()

      var i = 0 // for s
      var j = 0 // for p

      while (i < t.length) {
        if (pattern(j) == t.charAt(i)) {
          i += 1
          j += 1
          comparasions += 1
        }
        if (j == pattern.length) {
          println(s"pattern found at ${i - j}")
          result += (i - j)
          //match found
          j = lookupTable(j - 1)
        }
        else {
          if (i < t.length && t.charAt(i) != pattern(j)) {
            comparasions += 1
            if (j != 0){
              j = lookupTable(j - 1)
            }
            else i += 1
          }
        }
      }
    }
    result.toIterator
  }

  def toJson(text: CharSequence): String = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    var prefixHelper = getPrefixFun
    var prefixList = new ListBuffer[List[Int]]
    findAllIn(text)

    for (i <- prefixHelper.indices) {
      prefixList.append(List(i, prefixHelper(i)))
    }

    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> Json.toJson("KMP"),
      "pattern" -> Json.toJson(pattern),
      "text" -> Json.toJson(text.toString),
      "prefixFun" -> Json.toJson(prefixList.toList),
      "steps" -> Json.toJson(steps),
      "comparisons" -> Json.toJson(comparasions)
    )
    val reply = Json.stringify(Json.toJson(jsonMap))
    reply
  }
}