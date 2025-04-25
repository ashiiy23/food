import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Try

object Main {

  type FoodData = Map[String, List[Int]]
  type FoodBasket = Map[String, Float]

  // Food data structure loaded at the start
  val foodData: FoodData = loadData("data.txt")

  // Backend: Load data from file
  def loadData(filename: String): FoodData = {
    val lines = Source.fromFile(filename).getLines().toList
    lines.map { line =>
      val parts = line.split(",").map(_.trim)
      val name = parts.head.toUpperCase
      val prices = parts.tail.flatMap(s => Try(s.toInt).toOption).toList
      name -> prices
    }.toMap
  }
  

  // Backend: Current prices
  def getCurrentPrices(data: FoodData): Map[String, Int] =
    data.view.mapValues(_.last).toMap

  // Frontend: Show current prices
  def printCurrentPrices(data: FoodData): Unit = {
    println("\nCurrent Prices (Most Recent):")
    getCurrentPrices(data).foreach {
      case (food, price) => println(f"$food: £${price / 100.0}%.2f")
    }
  }

  // Backend: Highest and lowest prices
  def getHighLowPrices(data: FoodData): Map[String, (Int, Int)] =
    data.view.mapValues(prices => (prices.max, prices.min)).toMap

  // Frontend: Show high/low prices
  def printHighLowPrices(data: FoodData): Unit = {
    println("\nHighest and Lowest Prices:")
    getHighLowPrices(data).foreach {
      case (food, (high, low)) =>
        println(f"$food: High = £${high / 100.0}%.2f, Low = £${low / 100.0}%.2f")
    }
  }

  // Backend: Median calculation
  def median(lst: List[Int]): Double = {
    val sorted = lst.sorted
    val n = sorted.length
    if (n % 2 == 1) sorted(n / 2)
    else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0
  }

  def getMedians(data: FoodData): Map[String, Double] =
    data.view.mapValues(median).toMap

  // Frontend: Show median prices
  def printMedians(data: FoodData): Unit = {
    println("\nMedian Prices:")
    getMedians(data).foreach {
      case (food, med) => println(f"$food: £${med / 100.0}%.2f")
    }
  }

  // Backend: Average
  def average(lst: List[Int]): Double = lst.sum.toDouble / lst.length

  // Frontend: Compare average values
  def compareFoods(data: FoodData): Unit = {
    println("Enter first food symbol:")
    val f1 = readLine().trim.toUpperCase
    println("Enter second food symbol:")
    val f2 = readLine().trim.toUpperCase

    (data.get(f1), data.get(f2)) match {
      case (Some(l1), Some(l2)) =>
        val avg1 = average(l1)
        val avg2 = average(l2)
        println(f"\n$f1 average: £${avg1 / 100.0}%.2f")
        println(f"$f2 average: £${avg2 / 100.0}%.2f")
      case _ => println("One or both items not found.")
    }
  }

  // Backend: Get food basket
  def getBasketFromUser(data: FoodData): FoodBasket = {
    println("Enter items (SYMBOL,QUANTITY), 'done' to finish:")
    def loop(acc: FoodBasket): FoodBasket = {
      val input = readLine().trim
      if (input.toLowerCase == "done") acc
      else {
        input.split(",").map(_.trim) match {
          case Array(sym, qtyStr) =>
            Try(qtyStr.toFloat).toOption match {
              case Some(qty) if qty <= 0 =>
              println("Quantity must be a positive number."); loop(acc)
              case Some(qty) if data.contains(sym.toUpperCase) =>
                loop(acc + (sym.toUpperCase -> qty))
              case Some(_) =>
                println(s"$sym not found."); loop(acc)
              case None =>
                println("Invalid quantity."); loop(acc)
            }
          case _ =>
            println("Format: SYMBOL,QUANTITY"); loop(acc)
        }
      }
    }
    loop(Map.empty)
  }

  // Backend: Calculate basket total
  def calculateBasketTotal(basket: FoodBasket, data: FoodData): Double = {
    val currentPrices = getCurrentPrices(data)
    basket.collect {
      case (symbol, qty) if currentPrices.contains(symbol) =>
        currentPrices(symbol) * qty
    }.sum / 100.0
  }

  // Frontend: Show basket total
  def printBasketTotal(data: FoodData): Unit = {
    val basket = getBasketFromUser(data)
    val total = calculateBasketTotal(basket, data)
    println(f"\nTotal basket value: £$total%.2f")
  }

  // Menu
  def showMenu(): Unit = {
    println(
      """
        |======= Food Price Analysis Menu =======
        |1. Show current prices
        |2. Show highest and lowest prices
        |3. Show median prices
        |4. Compare average values of two foods
        |5. Calculate food basket total
        |6. Quit
        |========================================
        |Enter choice (1-6):
        |""".stripMargin)
  }

  // Main function
  def main(args: Array[String]): Unit = {
    def loop(): Unit = {
      showMenu()
      readLine().trim match {
        case "1" => printCurrentPrices(foodData); loop()
        case "2" => printHighLowPrices(foodData); loop()
        case "3" => printMedians(foodData); loop()
        case "4" => compareFoods(foodData); loop()
        case "5" => printBasketTotal(foodData); loop()
        case "6" => println("Exiting...")
        case _ => println("Invalid choice."); loop()
      }
    }
    loop()
  }
}
