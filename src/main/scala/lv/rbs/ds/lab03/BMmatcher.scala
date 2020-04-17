package main.scala.lv.rbs.ds.lab03

import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable.ListBuffer

class BMmatcher(pattern1:String) {
  //pattern to chararr
  var pattern = pattern1.toCharArray

  var comparasions:Int = 0
  var steps:List[Map[String, String]] = List()

  var ASCII_SIZE: Int = 256

  // function to get maximum of two integers
  def max(a: Int, b: Int): Int = Math.max(a, b)

  def findAllIn(txt1: String): List[Int] = {
    var result:List[Int] = List()
    var txt = txt1.toCharArray
    val m: Int = pattern.length
    val n: Int = txt.length
    // initiate array of bad characters
    val badchar: Array[Int] = Array.ofDim[Int](ASCII_SIZE)
    for (i <- 0 until ASCII_SIZE) {
      badchar(i) = -1
    }
    for (i <- 0 until m) {
      badchar(pattern(i).toInt) = i
    }
    // s is shift of the pattern
    var s: Int = 0
    // respect to text
    while (s <= (n - m)) {
      var j: Int = m - 1
      // keep reducing index j of pattern while characters of pattern and text are matching at this shift s
      while (j >= 0 && pattern(j) == txt(s + j)) j -= 1

      // If the pattern is present at current shift, then index j will become -1 after the above loop
      if (j < 0) {
        result = result :+ s
        steps = steps :+ Map("offset" -> s.toString,
          "end" -> (j + 1).toString,
          "match" -> "true")
        // shift the pattern so that the next character in text aligns with the last occurrence of it in pattern.
        // the condition s + m < n is necessary for the case when pattern occurs at the end of text
        if (s + m < n) s += m - badchar(txt(s + m))
        else s += 1
      }
      else {
        steps = steps :+ Map("offset" -> s.toString,
          "end" -> j.toString)
        // Shift the pattern so that the bad character in text aligns with the last occurrence of it in pattern.
        // The max function is used to make sure that we get a positive shift. We may get a negative shift if the last
        // occurrence  of bad character in pattern is on the right side of the current character.
        s += max(1, j - badchar(txt(s + j)))
      }
    }
    result
  }

  def getGoodSuffixFun(): List[(Int,Int)] = {
    val m: Int = pattern.length
    val bpos: Array[Int] = Array.ofDim[Int](m + 1)
    val shift: Array[Int] = Array.ofDim[Int](m + 1)
    for (i <- 0 until m + 1) shift(i) = 0
    var i: Int = m
    var j: Int = m + 1
    bpos(i) = j
    while (i > 0) {
      /*if character at position i-1 is not equivalent to character at j-1,
        then continue searching to right of the pattern for border */
      while (j <= m && pattern(i - 1) != pattern(j - 1)) {
        /* the character preceding the occurrence of t in pattern P is different than the mismatching
            character in P, we stop skipping the occurrences and shift the pattern from i to j */

        if (shift(j) == 0) shift(j) = j - i
        //Update the position of next border
        j = bpos(j)
      }
      /* p[i-1] matched with p[j-1], border is found.
        store the beginning position of border */
      i -= 1
      j -= 1
      bpos(i) = j
    }
    j = bpos(0)

    i = 0
    while (i <= m) {
      /* set the border position of the first character
        of the pattern to all indices in array shift
        having shift[i] = 0 */

      if (shift(i) == 0) shift(i) = j
      /* suffix becomes shorter than bpos[0],
        use the position of next widest border
        as value of j */

      if (i == j) j = bpos(j)
      i += 1
    }
    shift.toList

    var answer:List[(Int, Int)] = List()
    var counter = 0

    for(i <- shift){
      answer = answer :+ (counter, i)
        counter += 1
    }
    answer
  }

  def getBadCharFun(): List[(Char, Int)] = {
    val badchar: Array[Int] = Array.ofDim[Int](ASCII_SIZE)
    for (i <- pattern.indices) {
      badchar(pattern(i).toInt) = i
    }
    val a = badchar.filter(_ > 0)

    var list: List[(Char, Int)] = List()
    for (i <- badchar.indices) {
      if (badchar(i) > 0) {
        list = list :+ (i.toChar, badchar(i))
      }
    }
    list
  }

  def toJson(text: String): String = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    var sufix = getGoodSuffixFun()
    findAllIn(text)

    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> Json.toJson("BM"),
      "pattern" -> Json.toJson(pattern1),
      "text" -> Json.toJson(text.toString),
      "prefixFun" -> Json.toJson(sufix),
      "steps" -> Json.toJson(steps),
      "comparisons" -> Json.toJson(comparasions)
    )
    val reply = Json.stringify(Json.toJson(jsonMap))
    reply
  }
}