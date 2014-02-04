/**
  * JugglerAssigner.scala
  * 
  * Solution to the problem described at http://www.yodlecareers.com/puzzles/jugglefest.html
  *
  * Author: Charlie Flowers 2/4/14
  * Written in Scala (http://www.scala-lang.org) using Scala IDE for Eclipse (http://www.scala-ide.org)
  * Execute in the Scala interpreter using the command "scala JugglerAssigner.scala -e JugglerAssigner"
  */

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL

import scala.collection.immutable.TreeMap

object JugglerAssigner {

  val SRC_URL = "http://www.yodlecareers.com/puzzles/jugglefest.txt"

  // Regexp for incoming circuit and juggler data, inferred from problem description
  val cFormat = new scala.util.matching.Regex("""^C (\S+) H:(\d{1,2}) E:(\d{1,2}) P:(\d{1,2}).*$""", "name", "h", "e", "p")
  val jFormat = new scala.util.matching.Regex("""^J (\S+) H:(\d{1,2}) E:(\d{1,2}) P:(\d{1,2}) (\S+).*$""", "name", "h", "e", "p", "preferences")

  // Comparator to keep names ending in numbers sorted intuitively by the numerical portion of the name
  object nameSort extends Ordering[String] {
    def compare(a: String, b: String): Int =
      {
        val nFormat = new scala.util.matching.Regex("""^\D*(\d+)$""", "numValue")
        try {
          val nFormat(num1) = a
          val nFormat(num2) = b

          return num1.toInt.compare(num2.toInt)
        }
        catch { // Name does not match expected format - use default comparator instead.
          case e: scala.MatchError => return a.compare(b)
          case e: NumberFormatException => return a.compare(b)
        }
      }
  }
  // Store circuits & jugglers in sorted Map indexed by name for ease of lookup
  var circuits = TreeMap[String, Circuit]()(nameSort)

  var jugglers = TreeMap[String, Juggler]()(nameSort)

  def main(args: Array[String]): Unit = {

    // Read input from provided URL
    var httpIn = new BufferedReader(new InputStreamReader(new URL(SRC_URL).openStream()))

    var line: String = httpIn.readLine()

    while (line != null) {

      if (line.startsWith("C")) {
        val circuit = new Circuit(line)

        circuits += (circuit.name -> circuit)
      }
      else if (line.startsWith("J")) {
        val juggler = new Juggler(line)

        jugglers += (juggler.name -> juggler)
      }

      line = httpIn.readLine()
    }

    httpIn.close() // All input parsed; close connection

    jugglers.foreach(juggler => juggler._2.circuitAssign(0)) // Attempt to assign each juggler to his favorite circuit

    // All assignments complete - print results    
    circuits.foreach(c => println(c._2.toString))

    return
  }

  class Juggler(jString: String) {

    val jFormat(name, hString, eString, pString, prefList) = jString

    val h = hString.toInt
    val e = eString.toInt
    val p = pString.toInt
    val affinities = prefList.split(",")

    def dotProd(c: Circuit): Int =
      {
        return (this.h * c.h) + (this.e * c.e) + (this.p * c.p)
      }

    // Recursively find the best circuit for each juggler based on preference; iterate through all circuits for placement if no preference fits
    def circuitAssign(priority: Int): Int = {

      if (priority < affinities.size) { // This juggler has preferred circuits to choose from - try to find a fit
        val circuit = circuits(affinities(priority))

        val insertResult = circuit.tryInsert(name, dotProd(circuit))

        if (insertResult < 0)
          return circuitAssign(priority + 1) // No room in the chosen circuit - try his next choice
        else
          return insertResult // Successful placement!
      }
      else // Overly optimistic - no room in any of his choices, stick him wherever we can
      {

        var fitFound = false
        val iter = circuits.valuesIterator
        while (iter.hasNext && !fitFound) {
          val course = iter.next

          if (course.tryInsert(name, dotProd(course)) >= 0) {
            fitFound = true
          }
        }

        if (!fitFound) { // Something's gone wrong - no room for this guy
          throw new Error(name + " needs a new hobby (failed all circuits)!")
        }
        else
          return 1
      }
    }

  }

  class Circuit(cString: String) {
    val cFormat(name, hString, eString, pString) = cString

    val h = hString.toInt
    val e = eString.toInt
    val p = pString.toInt

    var assigned = Array[(String, Int)]()

    def tryInsert(name: String, score: Int): Int = {

      val circuitMax = jugglers.size / circuits.size

      for (i <- 0 until circuitMax) { // Circuit not full - append to circuit by default
        if (i >= assigned.length) {
          assigned = assigned :+ (name, score)

          return i
        }
        else if (score > assigned(i)._2) { // Spot earned on merit - insert and bump poorest fit if necessary
          val tmp = assigned.splitAt(i)
          assigned = tmp._1 :+ (name, score)
          assigned = assigned ++ tmp._2

          if (assigned.length > circuitMax) { // Circuit's now overbooked - dump the poorest fit
            var bumpedGuy = jugglers(assigned.last._1)

            assigned = assigned.take(circuitMax) // Trim the array to the prescribed size (e.g. remove the poorest fit)...

            bumpedGuy.circuitAssign( // ...attempt to reassign him, to either...
              if (bumpedGuy.affinities.indexOf(this.name) >= 0) // ...a) his next preference, or ...
                bumpedGuy.affinities.indexOf(this.name) + 1
              else // ...b) any available circuit, if no preferences fit...
                bumpedGuy.affinities.length + 1) // ...indicated by a priority outside of the range of his choices
          }

          return i
        }
      }

      return -1
    }

    override def toString(): String = { // Override toString() to list course assignments in specified format
      var cString: String = name + " "
      for (i <- 0.to(assigned.size - 1)) {
        val j = jugglers(assigned(i)._1)

        cString += j.name

        j.affinities.foreach(x => cString += " " + x + ":" + j.dotProd(circuits(x)))

        if (i < assigned.size - 1) cString += ","
      }

      return cString
    }

  }

}