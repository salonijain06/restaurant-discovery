package restaurant

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import org.mongodb.scala.Document
import org.mongodb.scala.bson._

import scala.jdk.CollectionConverters._
import scala.util.Try

/** Custom exception used for bad user input (exception handling requirement). */
class ValidationException(message: String) extends Exception(message)

/** Trait: anything that can be printed nicely in the CLI. */
trait Displayable {
  def display: String
}

final case class Address(building: String, street: String, zipcode: String, coord: List[Double])

final case class Grade(dateMillis: Option[Long], grade: String, score: Option[Int]) {
  def dateText: String =
    dateMillis.map(m => Grade.formatter.format(Instant.ofEpochMilli(m))).getOrElse("n/a")

  def toBson: BsonDocument =
    Document(
      "date"  -> BsonDateTime(dateMillis.getOrElse(System.currentTimeMillis())),
      "grade" -> grade,
      "score" -> score.getOrElse(0)
    ).toBsonDocument
}

object Grade {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.of("UTC"))

  def now(grade: String, score: Int): Grade = Grade(Some(System.currentTimeMillis()), grade, Some(score))
}

final case class Restaurant(
    restaurantId: String,
    name: String,
    borough: String,
    cuisine: String,
    address: Address,
    grades: List[Grade]
) extends Displayable {

  def latestGrade: Option[Grade] = grades.headOption // dataset stores newest grade first

  def averageScore: Option[Double] = {
    val scores = grades.flatMap(_.score)
    if (scores.isEmpty) None else Some(scores.sum.toDouble / scores.size)
  }

  override def display: String = {
    val latest = latestGrade
      .map(g => s"${g.grade} (score ${g.score.fold("n/a")(_.toString)}) on ${g.dateText}")
      .getOrElse("no grades")
    val avg = averageScore.map(a => f"$a%.1f").getOrElse("n/a")
    s"""[$restaurantId] $name | $cuisine | $borough
       |    ${address.building} ${address.street}, ${address.zipcode}
       |    Latest grade: $latest | Avg score: $avg | Inspections: ${grades.size}""".stripMargin
  }

  def toDocument: Document =
    Document(
      "restaurant_id" -> restaurantId,
      "name"          -> name,
      "borough"       -> borough,
      "cuisine"       -> cuisine,
      "address" -> Document(
        "building" -> address.building,
        "street"   -> address.street,
        "zipcode"  -> address.zipcode,
        "coord"    -> BsonArray(address.coord.map(c => BsonDouble(c)))
      ),
      "grades" -> BsonArray(grades.map(_.toBson))
    )
}

object Restaurant {

  private val Boroughs = List("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "Missing")

  /** Option + pattern matching: turn free text into a valid borough name. */
  def normaliseBorough(input: String): Option[String] =
    Boroughs.find(_.equalsIgnoreCase(input.trim))

  def titleCase(s: String): String =
    s.trim.split("\\s+").filter(_.nonEmpty).map(w => w.head.toUpper + w.tail.toLowerCase).mkString(" ")

  /** Factory that validates every field and throws ValidationException when something is wrong. */
  def create(
      id: String,
      name: String,
      borough: String,
      cuisine: String,
      building: String,
      street: String,
      zipcode: String,
      grades: List[Grade] = Nil
  ): Restaurant = {
    if (name.trim.isEmpty) throw new ValidationException("Name cannot be empty.")
    if (cuisine.trim.isEmpty) throw new ValidationException("Cuisine cannot be empty.")
    if (!zipcode.matches("\\d{5}")) throw new ValidationException("ZIP code must be exactly 5 digits.")
    val b = normaliseBorough(borough).getOrElse(
      throw new ValidationException(s"Borough must be one of: ${Boroughs.mkString(", ")}.")
    )
    Restaurant(id, name.trim, b, titleCase(cuisine), Address(building.trim, street.trim, zipcode, Nil), grades)
  }

  // ---- Document -> Restaurant (defensive: the sample data has missing fields) ----

  private def str(b: BsonDocument, key: String): String =
    Option(b.get(key)).filter(_.isString).map(_.asString.getValue).getOrElse("")

  def fromDocument(d: Document): Option[Restaurant] = Try {
    val b = d.toBsonDocument
    val addr = Option(b.get("address")).filter(_.isDocument).map(_.asDocument).getOrElse(new BsonDocument())
    val coords = Option(addr.get("coord"))
      .filter(_.isArray)
      .map(_.asArray.getValues.asScala.toList.filter(_.isNumber).map(_.asNumber.doubleValue))
      .getOrElse(Nil)
    val grades = Option(b.get("grades"))
      .filter(_.isArray)
      .map(_.asArray.getValues.asScala.toList.filter(_.isDocument).map { v =>
        val g = v.asDocument
        Grade(
          Option(g.get("date")).filter(_.isDateTime).map(_.asDateTime.getValue),
          str(g, "grade"),
          Option(g.get("score")).filter(_.isNumber).map(_.asNumber.intValue)
        )
      })
      .getOrElse(Nil)

    Restaurant(
      str(b, "restaurant_id"),
      str(b, "name"),
      str(b, "borough"),
      str(b, "cuisine"),
      Address(str(addr, "building"), str(addr, "street"), str(addr, "zipcode"), coords),
      grades
    )
  }.toOption
}