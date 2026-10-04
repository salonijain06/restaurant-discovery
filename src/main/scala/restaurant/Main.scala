package restaurant

import com.mongodb.MongoException
import org.mongodb.scala.Document
import org.mongodb.scala.bson.BsonValue

import scala.io.StdIn
import scala.util.Try
import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]): Unit = {
    // Local MongoDB by default; set MONGO_URI for Atlas, e.g. mongodb+srv://user:pass@cluster.mongodb.net
    val uri = sys.env.getOrElse("MONGO_URI", "mongodb://localhost:27017")
    val ctx = new MongoContext(uri)
    try {
      val repo      = new MongoRestaurantRepository(ctx)
      val analytics = new AnalyticsService(ctx)
      val indexes   = new IndexManager(ctx)

      println("Connecting and making sure indexes exist ...")
      indexes.ensureIndexes().foreach(println)
      println(s"Connected. ${repo.count()} restaurants in sample_restaurants.restaurants\n")

      new Cli(repo, analytics, indexes).run()
    } catch {
      case e: MongoException => println(s"Could not talk to MongoDB: ${e.getMessage}")
      case NonFatal(e)       => println(s"Fatal error: ${e.getMessage}")
    } finally ctx.close()
  }
}

class Cli(repo: RestaurantRepository, analytics: AnalyticsService, indexes: IndexManager) {

  private val PageSize = 10

  // ------------------------------------------------------------------
  // main loop
  // ------------------------------------------------------------------
  def run(): Unit = {
    var running = true
    while (running) {
      printMenu()
      ask("Choose an option") match {
        case "1" => safely(addMenu())
        case "2" => safely(searchMenu())
        case "3" => safely(updateMenu())
        case "4" => safely(deleteMenu())
        case "5" => safely(analyticsMenu())
        case "6" => safely(indexMenu())
        case "7" =>
          running = false
          println("Goodbye!")
        case other => println(s"Unknown option '$other'.")
      }
    }
  }

  private def printMenu(): Unit =
    println(
      """
        |========== RESTAURANT DISCOVERY & ANALYTICS ==========
        | 1. Add Restaurant
        | 2. Search / View Restaurants
        | 3. Update Restaurant
        | 4. Delete Restaurant
        | 5. Restaurant Analytics
        | 6. Index Information
        | 7. Exit
        |======================================================""".stripMargin
    )

  /** Central error handling so one failure never kills the program. */
  private def safely(action: => Unit): Unit =
    try action
    catch {
      case e: ValidationException => println(s"Invalid input: ${e.getMessage}")
      case e: MongoException      => println(s"Database error: ${e.getMessage}")
      case NonFatal(e)            => println(s"Unexpected error: ${e.getMessage}")
    }

  // ------------------------------------------------------------------
  // input helpers
  // ------------------------------------------------------------------
  private def ask(prompt: String): String =
    Option(StdIn.readLine(s"$prompt: ")).map(_.trim).getOrElse {
      println("\nInput closed. Bye!")
      sys.exit(0)
    }

  private def askInt(prompt: String): Option[Int] = Try(ask(prompt).toInt).toOption

  private def newId(): String = s"NEW${System.currentTimeMillis()}"

  private def showAll(rs: Seq[Restaurant]): Unit =
    if (rs.isEmpty) println("No restaurants found.")
    else {
      rs.foreach(r => println(r.display))
      println(s"-- ${rs.size} result(s) (max $PageSize shown) --")
    }

  // ------------------------------------------------------------------
  // 1. CREATE
  // ------------------------------------------------------------------
  private def addMenu(): Unit =
    ask("1) Enter a restaurant manually   2) Insert 2 demo restaurants") match {
      case "1" => addManual()
      case "2" => addDemo()
      case _   => println("Cancelled.")
    }

  private def addManual(): Unit = {
    val name     = ask("Name")
    val cuisine  = ask("Cuisine")
    val borough  = ask("Borough (Bronx/Brooklyn/Manhattan/Queens/Staten Island)")
    val building = ask("Building number")
    val street   = ask("Street")
    val zip      = ask("ZIP code (5 digits)")
    val gradeTxt = ask("Initial grade A/B/C (blank = none)").toUpperCase
    val grades = gradeTxt match {
      case "" => Nil
      case g @ ("A" | "B" | "C") =>
        val score = askInt("Inspection score (number)").getOrElse(
          throw new ValidationException("Score must be a number.")
        )
        List(Grade.now(g, score))
      case other => throw new ValidationException(s"Unknown grade '$other'.")
    }
    val r = Restaurant.create(newId(), name, borough, cuisine, building, street, zip, grades)
    repo.add(r)
    println("Added:\n" + r.display)
  }

  private def addDemo(): Unit = {
    val demos = List(
      Restaurant("DEMO001", "Gujarati Thali House", "Queens", "Indian",
        Address("101", "Roosevelt Ave", "11372", List(-73.89, 40.75)), List(Grade.now("A", 7))),
      Restaurant("DEMO002", "Sunrise Dosa Cafe", "Brooklyn", "Indian",
        Address("202", "Coney Island Ave", "11230", List(-73.96, 40.62)), List(Grade.now("A", 9)))
    )
    demos.foreach { r =>
      if (repo.findById(r.restaurantId).isDefined) println(s"${r.restaurantId} already exists - skipped.")
      else {
        repo.add(r)
        println("Added:\n" + r.display)
      }
    }
  }

  // ------------------------------------------------------------------
  // 2. READ / SEARCH
  // ------------------------------------------------------------------
  private def searchMenu(): Unit = {
    println(
      """ 1) By name (contains)
        | 2) By cuisine and/or borough
        | 3) By ZIP code
        | 4) By inspection score (>= N)
        | 5) By restaurant id
        | 6) Total count""".stripMargin
    )
    ask("Search type") match {
      case "1" => showAll(repo.searchByName(ask("Name contains"), PageSize))
      case "2" =>
        val cuisine = Option(ask("Cuisine (blank = any)")).filter(_.nonEmpty).map(Restaurant.titleCase)
        val boroughTxt = ask("Borough (blank = any)")
        val borough =
          if (boroughTxt.isEmpty) None
          else Some(Restaurant.normaliseBorough(boroughTxt).getOrElse(
            throw new ValidationException("Unknown borough.")))
        showAll(repo.searchByCuisineAndBorough(cuisine, borough, PageSize))
      case "3" => showAll(repo.searchByZip(ask("ZIP code"), PageSize))
      case "4" =>
        val n = askInt("Minimum score").getOrElse(throw new ValidationException("Score must be a number."))
        showAll(repo.searchByMinScore(n, PageSize))
      case "5" =>
        repo.findById(ask("Restaurant id")) match {
          case Some(r) => println(r.display)
          case None    => println("Not found.")
        }
      case "6" => println(s"Total restaurants: ${repo.count()}")
      case _   => println("Cancelled.")
    }
  }

  // ------------------------------------------------------------------
  // 3. UPDATE
  // ------------------------------------------------------------------
  private def updateMenu(): Unit = {
    val id = ask("Restaurant id to update")
    repo.findById(id) match {
      case None => println("Not found.")
      case Some(r) =>
        println("Current record:\n" + r.display)
        println(" 1) Rename   2) Change cuisine   3) Change borough   4) Change ZIP   5) Add inspection grade")
        ask("What to change") match {
          case "1" => report(repo.updateField(id, "name", ask("New name")))
          case "2" => report(repo.updateField(id, "cuisine", Restaurant.titleCase(ask("New cuisine"))))
          case "3" =>
            val b = Restaurant.normaliseBorough(ask("New borough")).getOrElse(
              throw new ValidationException("Unknown borough."))
            report(repo.updateField(id, "borough", b))
          case "4" =>
            val z = ask("New ZIP")
            if (!z.matches("\\d{5}")) throw new ValidationException("ZIP must be 5 digits.")
            report(repo.updateField(id, "zipcode", z))
          case "5" =>
            val g = ask("Grade (A/B/C)").toUpperCase
            if (!Set("A", "B", "C").contains(g)) throw new ValidationException("Grade must be A, B or C.")
            val s = askInt("Score").getOrElse(throw new ValidationException("Score must be a number."))
            report(repo.addGrade(id, Grade.now(g, s)))
          case _ => println("Cancelled.")
        }
        repo.findById(id).foreach(u => println("Updated record:\n" + u.display))
    }
  }

  private def report(ok: Boolean): Unit = println(if (ok) "Update successful." else "Nothing was updated.")

  // ------------------------------------------------------------------
  // 4. DELETE
  // ------------------------------------------------------------------
  private def deleteMenu(): Unit = {
    val id = ask("Restaurant id to delete")
    repo.findById(id) match {
      case None => println("Not found.")
      case Some(r) =>
        println(r.display)
        if (ask("Really delete this restaurant? (y/N)").equalsIgnoreCase("y"))
          println(if (repo.delete(id)) "Deleted." else "Delete failed.")
        else println("Cancelled.")
    }
  }

  // ------------------------------------------------------------------
  // 5. ANALYTICS
  // ------------------------------------------------------------------
  private def analyticsMenu(): Unit = {
    println(
      """ 1) Restaurants per cuisine (top 10)
        | 2) Restaurants per borough
        | 3) Average inspection score per cuisine (lower = better)
        | 4) Top-rated restaurants (lowest average score)
        | 5) Grade distribution
        | 6) Run all""".stripMargin
    )
    val choice = ask("Report")
    def show(title: String, rows: Seq[Document]): Unit = printRows(title, rows)
    def r1() = show("Restaurants per cuisine", analytics.restaurantsByCuisine(10))
    def r2() = show("Restaurants per borough", analytics.restaurantsByBorough())
    def r3() = show("Average score per cuisine (min 100 inspections)", analytics.avgScoreByCuisine(100, 10))
    def r4() = show("Top-rated restaurants (min 3 inspections)", analytics.topRatedRestaurants(3, 10))
    def r5() = show("Grade distribution", analytics.gradeDistribution())
    choice match {
      case "1" => r1()
      case "2" => r2()
      case "3" => r3()
      case "4" => r4()
      case "5" => r5()
      case "6" => r1(); r2(); r3(); r4(); r5()
      case _   => println("Cancelled.")
    }
  }

  private def fmt(v: BsonValue): String = v match {
    case s if s.isString => s.asString.getValue
    case n if n.isInt32  => n.asInt32.getValue.toString
    case n if n.isInt64  => n.asInt64.getValue.toString
    case n if n.isNumber => f"${n.asNumber.doubleValue}%.2f"
    case other           => other.toString
  }

  private def printRows(title: String, rows: Seq[Document]): Unit = {
    println(s"\n--- $title ---")
    if (rows.isEmpty) println("(no data)")
    rows.zipWithIndex.foreach { case (d, i) =>
      val line = d.iterator.map { case (k, v) => s"${if (k == "_id") "key" else k}: ${fmt(v)}" }.mkString(" | ")
      println(f"${i + 1}%2d. $line")
    }
  }

  // ------------------------------------------------------------------
  // 6. INDEXES
  // ------------------------------------------------------------------
  private def indexMenu(): Unit = {
    println(" 1) Create / verify indexes   2) List indexes   3) Prove indexes are used (explain)")
    ask("Choice") match {
      case "1" => indexes.ensureIndexes().foreach(println)
      case "2" =>
        indexes.listIndexes().foreach { d =>
          val b = d.toBsonDocument
          println(s"- ${b.getString("name").getValue}  keys=${b.getDocument("key").toJson}")
        }
      case "3" =>
        indexes.demoQueries.foreach { case (label, filter) =>
          println(f"$label%-32s -> ${indexes.explain(filter)}")
        }
      case _ => println("Cancelled.")
    }
  }
}