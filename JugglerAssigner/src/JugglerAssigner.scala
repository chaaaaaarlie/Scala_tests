/**
  * JugglerAssigner.scala
  *
  * Solution to the problem described at http://www.yodlecareers.com/puzzles/jugglefest.html
  *
  * Written in Scala (http://www.scala-lang.org) using Scala IDE for Eclipse (http://www.scala-ide.org)
  * Execute in the Scala interpreter using the command "scala JugglerAssigner.scala"
  *
  * Notes on decisions made to handle cases not directly addressed in the problem specification at the above URL:
  *
  * 1- When two jugglers are equally suited to a given circuit, priority will be given to the juggler who has the
  *    circuit higher in his list of preferences.
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
  *       which seems to imply that a juggler should not displace a better-fit juggler in any case, even if that juggler was shifted into that
  *       circuit arbitrarily.
  *
  * Author: Charlie Flowers
  * Date: 2/5/14
  */

// Exists in default package for compatibility with the Scala interpreter.

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.net.URL
import scala.collection.immutable.TreeMap
import java.io.FileReader
import java.io.File
import java.io.FileNotFoundException

object JugglerAssigner {

  val SRC_URL = "http://www.yodlecareers.com/puzzles/jugglefest.txt" // Default source for input file, as provided in problem description.

  // Regex for incoming circuit and juggler data, inferred from problem description
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

    /* Read input either from filename provided as argument, or, if not provided, from URL provided in problem description 
     * 
     * Fails if arg(0) is not a valid file.
     */
    val fileIn = if (args.length > 0)
      new BufferedReader(new FileReader(new File(args(0))))
    else
      new BufferedReader(new InputStreamReader(new URL(SRC_URL).openStream))

    var line: String = fileIn.readLine

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

      line = fileIn.readLine
    }

    fileIn.close // All input parsed; close connection

    jugglers.foreach(juggler => juggler._2.circuitAssign(0)) // Attempt to assign each juggler to his favorite (priority 0) circuit

    // All assignments complete - print results to stdout
    circuits.foreach(c => println(c._2.toString))

    return
  }

  class Juggler(jString: String) {

    val jFormat(name, hString, eString, pString, prefList) = jString // Parse input line into provided arguments

    val h = hString.toInt // Strings to Ints...
    val e = eString.toInt
    val p = pString.toInt
    val preferredCircuits = prefList.split(",") // ...and to Array

    /* Important - Since many jugglers may be equally qualified for the same course, we here add a weighted value between 0 and 1 to indicate the
     * juggler's level of interest, so that jugglers with a strong interest in a particular circuit are given priority over equally 
     * well-suited jugglers with a weaker interest. This is probably overkill, but adds negligible overhead to the calculation.
     */
    def weightedDotProd(c: Circuit): Double =
      {
        return dotProd(c) + 1.0 - (preferredCircuits.indexOf(c.name) + 1.0) / (preferredCircuits.length + 1.0)
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

      if (prefLevel < preferredCircuits.size) { // This juggler has preferred circuits to choose from - try to find a fit
        val circuit = circuits(preferredCircuits(prefLevel))

        val insertResult = circuit.tryInsert(name, weightedDotProd(circuit)) // Try to find room in the circuit based on the juggler's suitability

        if (!insertResult)
          return circuitAssign(prefLevel + 1) // No room in the chosen circuit - try the next choice
        else
          return insertResult // Successful placement!
      }
      else // Poor chooser - doesn't qualify for any of his choices
      {

        // Reduce the list of circuits to the single circuit for which this candidate is best suited.
        val bestFit = circuits.values.reduce { (bestCircuit, thisCircuit) =>
          val champ = bestCircuit.fitsAt(dotProd(bestCircuit))
          val contender = thisCircuit.fitsAt(dotProd(thisCircuit))

          if (contender < 0) // No fit in this circuit - best circuit remains the same.
            bestCircuit;
          else if (champ < 0) // No fit found until this circuit - best circuit is this one.
            thisCircuit;
          else if (contender < champ) // Candidate places higher in this circuit than previous best - best circuit is this one.
            thisCircuit;
          else if (champ < contender) // Candidate fails to place higher here than in best - best circuit remains the same.
            bestCircuit;
          else if (dotProd(bestCircuit) >= dotProd(thisCircuit)) // Tiebreaker - If placement in this circuit is equal to best, go with the one where the candidate scores higher.
            bestCircuit;
          else
            thisCircuit;
        }

        if (bestFit.tryInsert(name, dotProd(bestFit))) // No preference - use the pure dot product for calculation
          return true
        else // Something's gone wrong - this guy doesn't fit anywhere!
          throw new Error(name + " needs a new hobby (failed all circuits)!")
      }
    }
  }

  class Circuit(cString: String) {
    val cFormat(name, hString, eString, pString) = cString // Parse input line into provided arguments

    // Convert Strings to Ints
    val h = hString.toInt
    val e = eString.toInt
    val p = pString.toInt

    var assigned = Array[(String, Double)]()

    // Determine where to slot a juggler with a given score, or -1 if he doesn't qualify.
    def fitsAt(score: Double): Int =
      {
        val circuitMax: Int = jugglers.size / circuits.size // Maximum number of jugglers allowed in each circuit

        for (i <- 0 until circuitMax) {
          if (i >= assigned.length) // Circuit not full - append candidate to the end
            return assigned.length
          else if (score > assigned(i)._2) // Candidate qualifies on merit - insert him here.
            return i
        }

        return -1
      }
    /* Attempt to insert the named juggler with the given score into this circuit. If necessary, relocate a poorer candidate to
     * a more suitable circuit.
     */
    def tryInsert(name: String, score: Double): Boolean = {

      val circuitMax: Int = jugglers.size / circuits.size // Maximum number of jugglers allowed in each circuit

      val insertPos = fitsAt(score)

      if (insertPos >= 0) {
        val tmp = assigned.splitAt(insertPos) // Split array at insertion point, reconstruct with newly inserted member
        assigned = tmp._1 :+ (name, score)
        assigned = assigned ++ tmp._2

        if (assigned.length > circuitMax) { // Circuit's now overbooked - dump the poorest fit
          var bumpedGuy = jugglers(assigned.last._1) // Identify the juggler to reassign...
          val bumpedGuysPrefLevel = bumpedGuy.preferredCircuits.indexOf(this.name) // ...as well as the degree of preference the relocating juggler had for this course
          assigned = assigned.take(circuitMax) // Trim the array to the prescribed size (e.g. remove the poorest fit)

          bumpedGuy.circuitAssign( // Attempt to reassign, either to...
            if (bumpedGuysPrefLevel >= 0) // ...a) his next-lower preference, or ...
              bumpedGuysPrefLevel + 1
            else // ...b) the most suitable circuit, if no preferences remain...
              bumpedGuy.preferredCircuits.length + 1) // ...indicated by passing a priority outside of the range of his choices
        }

        return true // Note the successful insertion
      }
      else
        return false // This juggler's score did not qualify in this circuit - return insertion failure
    }

    override def toString(): String = { // Override toString() to list course assignments in specified format
      var cString: String = name + " "

      val iter = assigned.iterator
      while (iter.hasNext) {
        val j = jugglers(iter.next._1)

        cString += j.name
        cString += j.preferredCircuits.foldLeft("")((res, x) => res + " " + x + ":" + j.dotProd(circuits(x)))

        if (!j.preferredCircuits.contains(this.name)) // Juggler is assigned to a course not in his preferences - print the value for the course he wound up in
          cString += " (" + this.name + ":" + j.dotProd(this) + ")"

        if (iter.hasNext) cString += "," // Add comma only if there are more jugglers to be printed
      }
      return cString
    }
  }
}