import scala.collection.mutable
import scala.io.Source

object MyApp extends App {
  //declare start to measure time
  val start = System.currentTimeMillis()

  //source: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala/
  //importing csv file
  val data = Source.fromInputStream(MyApp.getClass.getResourceAsStream("/aefi.csv")).getLines.toArray

  //To retain order and remove duplicates
  //Source: https://www.scala-lang.org/api/2.12.19/scala/collection/mutable/LinkedHashSet.html
  val uniqueRowsSet = mutable.LinkedHashSet[String]()
  val duplicateCount = mutable.Map[String, Int]().withDefaultValue(0)

  for (row <- data) {
    if (!uniqueRowsSet.contains(row)) {
      uniqueRowsSet.add(row)
    }
    duplicateCount(row) += 1
  }

  //Convert the LinkedHashset back to array
  val uniqueData = uniqueRowsSet.toArray

  //to create map of key -> value where value is the sum of all values with the same key
  //used to total up total occurrences of each vaccine type
  //eg: total headaches, total vomits
  def mapAndCalcTotal(keyIndex: Int, valueIndex: Int): mutable.Map[String, Int] = {
    val map = mutable.Map[String, Int]()

    try{
      //source: https://alvinalexander.com/scala/csv-file-how-to-process-open-read-parse-in-scala/
      //iterate through every row and separate into columns
      //after separate, remove any whitespace
      for (row <- uniqueData.drop(1)) {
        val cols = row.split(",").map(_.trim)
        //source: https://chatgpt.com/
        //sum values with same key
        //getOrElse to set default value of 0 for keys that do not have a value
        val key: String = cols(keyIndex)
        val value: Int = cols(valueIndex).toInt
        map(key) = map.getOrElse(key, 0) + value
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Index " + keyIndex + " or " + valueIndex + " is out of bounds." +
        "Accepted range is between 0 to " + (uniqueData.head.split(",").map(_.trim).length - 1) + ".")
    }
    map
  }

  //used to count the frequency of each vaxtype
  def countFrequency(keyIndex:Int): mutable.Map[String, Int] = {
    val map = mutable.Map[String, Int]()
    try {
      for (row <- uniqueData.drop(1)) {
        val cols = row.split(",").map(_.trim)

        val key: String = cols(keyIndex)
        //value ++ when key is repeated
        map(key) = map.getOrElse(key, 0) + 1
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Index " + keyIndex + " is out of bounds." +
        "Accepted range is between 0 to " + (uniqueData.head.split(",").map(_.trim).length - 1) + ".")
    }
    map
  }

  //question 1
  //Which vaccination product is the most commonly used by Malaysian?
  println("Question 1")
  println("Which vaccination product is the most commonly used by Malaysian?")
  val vaxTypeDailyTotal = mapAndCalcTotal(1, 2)

  var mostUsedVaccineType: String = ""
  var highestCount: Int = 0

  for ((vaccine, count) <- vaxTypeDailyTotal) {
    if (count > highestCount) {
      highestCount = count
      mostUsedVaccineType = vaccine
    }
  }

  println(s"The most used vaccine type is $mostUsedVaccineType with a total of $highestCount vaccinations.")
  //print empty space for easy reading
  println()


  //question 2
  //What are the average occurrence of headache for each type of vaccination product in the provided data?
  println("Question 2")
  println("What are the average occurrence of headache for each type of vaccination product in the provided data?")

  //calculate total vaccinations and headaches for each vaxtype,
  val vaxTypeFreq = countFrequency(1)
  val vaxTypeHeadacheD1Total = mapAndCalcTotal(1, 12)
  val vaxTypeHeadacheD2Total = mapAndCalcTotal(1, 24)

  //calculate average occurence of headache for each vaxtype
  val vaxTypeHeadacheMean = mutable.Map[String, Double]()

  try {
    for ((vaxType, freq) <- vaxTypeFreq) {
      //average = (total d1_headache + total d2_headache) / frequency of vaccine
      vaxTypeHeadacheMean(vaxType) = (vaxTypeHeadacheD1Total.getOrElse(vaxType, 0) + vaxTypeHeadacheD2Total.getOrElse(vaxType, 0)) / freq.toDouble

      //source: https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch02s10.html
      //to print mean with 2 significant figures
      val mean = vaxTypeHeadacheMean(vaxType)
      println("Average occurrence of headaches for " + vaxType + " is " + f"$mean%.2f" + ".")
    }
  } catch {
    case e: ArithmeticException => println("Cannot divide by 0.")
  }

  //question 3
  //Which vaccination type has the highest occurrence of vomiting after first injection in the provided data?
  println()
  println("Question 3")
  println("Which vaccination type has the highest occurrence of vomiting after first injection in the provided data?")

  //Calculate total vomiting occurrences after the first injection for each vaccine type
  val vaxTypeVomitingD1Total = mapAndCalcTotal(1, 17)

  var highestVomitingVaccineType: String = ""
  var highestVomitingCount: Int = 0

  for ((vaccineType, countD1) <- vaxTypeVomitingD1Total) {
    if (countD1 > highestVomitingCount) {
      highestVomitingCount = countD1
      highestVomitingVaccineType = vaccineType
    }
  }

  println(s"The vaccine type with the highest occurrence of vomiting after the first injection is $highestVomitingVaccineType with a total of $highestVomitingCount occurrences.")

  //end to measure time
  val end = System.currentTimeMillis()

  //calculate time took to run code
  println("\nAll operations done in " + (end - start) + " ms.")
}