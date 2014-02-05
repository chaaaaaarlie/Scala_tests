/**
  * JugglerAssigner.scala
  *
  * Solution to the problem described at http://www.yodlecareers.com/puzzles/jugglefest.html
  *
  * Written in Scala (http://www.scala-lang.org) using Scala IDE for Eclipse (http://www.scala-ide.org)
  * Execute in the Scala interpreter using the command "scala JugglerAssigner.scala"
  *
  * Notes on decisions made to handle cases not addressed in the problem specification at the above URL:
  *
  * 1- When two jugglers are equally suited to a given circuit, priority will be given to the juggler who has the
  *    circuit listed higher in his list of preferences.
  *
  * 2- In cases where a juggler does not qualify for inclusion in any of his preferred circuits, he will be evaluated
  *    against all circuits and inserted wherever he places highest.
  *
  *    a) In these cases, the output format will be modified to include the juggler's score in his assigned course inside parentheses
  *       after the scores for his preferred courses
  *       
  *    b) This can result in jugglers who have no preference for a course displacing those that do. This choice has been made to
  *       satisfy the following line from the problem specification:
  *
  *       "...we want to match jugglers to circuits such that no juggler could switch to a circuit that they prefer more
  *       than the one they are assigned to and be a better fit for that circuit than one of the other jugglers assigned to it."
  *
  *       which seems to imply that a juggler should not displace a better-fit juggler in any case.
  *
  * Author: Charlie Flowers, 2/5/14
  */

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.net.URL

import scala.collection.immutable.TreeMap


object JugglerAssigner2 {

  val SRC_URL = "http://www.yodlecareers.com/puzzles/jugglefest.txt" // Location of input file

  // Regexp for incoming circuit and juggler data, inferred from problem description
  val cFormat = new scala.util.matching.Regex("""^C (\S+) H:(\d{1,2}) E:(\d{1,2}) P:(\d{1,2}).*$""", "name", "h", "e", "p")
  val jFormat = new scala.util.matching.Regex("""^J (\S+) H:(\d{1,2}) E:(\d{1,2}) P:(\d{1,2}) (\S+).*$""", "name", "h", "e", "p", "preferences")

  // Comparator to keep names ending in numbers sorted by the value of the numerical portion of the name
  object nameSort extends Ordering[String] {
    def compare(a: String, b: String): Int =
      {
        val nFormat = new scala.util.matching.Regex("""^\D*(\d+)$""", "numValue") // Expected name consists of any content that ends in at least one digit.
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
    val httpIn = new BufferedReader(new InputStreamReader(new URL(SRC_URL).openStream))

    val t0 = System.nanoTime()
    var line: String = httpIn.readLine

    while (line != null) {

      line = line.trim
      if (line.startsWith("C")) { // Construct circuit object and add to map
        val circuit = new Circuit(line)

        circuits += (circuit.name -> circuit)
      }
      else if (line.startsWith("J")) { // Construct juggler object and add to map
        val juggler = new Juggler(line)

        jugglers += (juggler.name -> juggler)
      }
      else if (line.length > 0) { // Unexpected data - throw exception and halt execution
        throw new IOException("Bad data - expected \"C ...\" or \"J ...\", got " + line)
      }

      line = httpIn.readLine
    }

    httpIn.close // All input parsed; close connection

    jugglers.foreach(juggler => juggler._2.circuitAssign(0)) // Attempt to assign each juggler to his favorite (priority 0) circuit

    // All assignments complete - print results to stdout
    circuits.foreach(c => println(c._2.toString))

    println("Execution time = " + (System.nanoTime() - t0) / Math.pow(10, 9) + " s")
    return
  }

  class Juggler(jString: String) {

    val jFormat(name, hString, eString, pString, prefList) = jString // Parse input line into provided arguments

    val h = hString.toInt // Strings to Ints...
    val e = eString.toInt
    val p = pString.toInt
    val preferredCourses = prefList.split(",") // ...and to Array

    /* Important - Since many jugglers may be equally qualified for the same course, we here add a weighted value between 0 and 1 to indicate the
     * juggler's level of interest, so that jugglers with a strong interest in a particular circuit are given priority over equally 
     * well-suited jugglers with a weaker interest. This is probably overkill, but adds negligible overhead to the calculation.
     */
    def weightedDotProd(c: Circuit): Double =
      {
        return dotProd(c) + 1.0 - (preferredCourses.indexOf(c.name) + 1.0) / (preferredCourses.length + 1.0)
      }
    // Unweighted dot product - used in cases where a juggler is to be assigned to a course for which he has not expressed a preference
    def dotProd(c: Circuit): Int =
      {
        return (this.h * c.h) + (this.e * c.e) + (this.p * c.p)
      }

    /* Recursively attempt to find a circuit for each juggler based on preference; when preferences are exhausted, iterate through all 
     * circuits for placement (this is indicated by passing a prefLevel outside of the range of the preferredCourses array) 
     */
    def circuitAssign(prefLevel: Int): Boolean = {

      if (prefLevel < preferredCourses.size) { // This juggler has preferred circuits to choose from - try to find a fit
        val circuit = circuits(preferredCourses(prefLevel))

        val insertResult = circuit.tryInsert(name, weightedDotProd(circuit)) // Try to find room in the circuit based on the juggler's suitability

        if (!insertResult)
          return circuitAssign(prefLevel + 1) // No room in the chosen circuit - try the next choice
        else
          return insertResult // Successful placement!
      }
      else // Poor chooser - doesn't qualify for any of his choices; iterate through all courses until a fit is found
      {    

        var fitFound = false
        
        
        val iter = circuits.values.iterator
        while (iter.hasNext && !fitFound) {
          val course = iter.next

          if (course.tryInsert(name, dotProd(course))) { // No preference - use the pure dot product for calculation
            fitFound = true
          }
        }

        if (!fitFound) { // Something's gone wrong - no room for this guy anywhere!
          throw new Error(name + " needs a new hobby (failed all circuits)!")
        }
        else
          return true
      }
    }
  }

  class Circuit(cString: String) {
    val cFormat(name, hString, eString, pString) = cString

    val h = hString.toInt
    val e = eString.toInt
    val p = pString.toInt

    var assigned = Array[(String, Double)]()

    /* Attempt to insert the named juggler with the given score into this circuit. If necessary, relocate a poorer candidate to
     * a more suitable circuit.
     */
    def tryInsert(name: String, score: Double): Boolean = {

      val circuitMax: Int = jugglers.size / circuits.size // Maximum number of jugglers allowed in each circuit

      for (i <- 0 until circuitMax) {

        if (i >= assigned.length) { // Circuit not full - append to circuit by default and return success
          assigned = assigned :+ (name, score)

          return true
        }
        else if (score > assigned(i)._2) { // Spot earned on merit - insert and bump poorest fit if necessary

          val tmp = assigned.splitAt(i) // Split array at insertion point, reconstruct with newly inserted member
          assigned = tmp._1 :+ (name, score)
          assigned = assigned ++ tmp._2

          if (assigned.length > circuitMax) { // Circuit's now overbooked - dump the poorest fit
            var bumpedGuy = jugglers(assigned.last._1) // Identify the juggler to reassign...
            assigned = assigned.take(circuitMax) // ...trim the array to the prescribed size (e.g. remove the poorest fit)...

            bumpedGuy.circuitAssign( // ...attempt to reassign him, to either...
              if (bumpedGuy.preferredCourses.indexOf(this.name) >= 0) // ...a) his next preference, or ...
                bumpedGuy.preferredCourses.indexOf(this.name) + 1
              else // ...b) any available circuit, if no preferences fit...
                bumpedGuy.preferredCourses.length + 1) // ...indicated by a priority outside of the range of his choices
          }

          return true // Note the successful insertion
        }
      }

      return false // This juggler's score did not qualify in this circuit - return insertion failure
    }

    override def toString(): String = { // Override toString() to list course assignments in specified format
      var cString: String = name + " "

      val iter = assigned.iterator
      while (iter.hasNext) {
        val j = jugglers(iter.next._1)
        var isPreferred = false

        cString += j.name
        j.preferredCourses.foreach { x =>
          cString += " " + x + ":" + j.dotProd(circuits(x));
          if (x.equals(this.name))
            isPreferred = true
        }

        if (!isPreferred) // Juggler is assigned to a course not in his preferences - print the value for the course he wound up in
          cString += " (" + this.name + ":" + j.dotProd(this) + ")"

        if (iter.hasNext) cString += "," // Add comma only if there are more jugglers to be printed
      }
      return cString
    }
  }
}