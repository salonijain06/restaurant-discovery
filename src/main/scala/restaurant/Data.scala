package restaurant

import java.util.regex.Pattern

import org.bson.conversions.Bson
import org.mongodb.scala._
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._

/** Encapsulates the connection. The client is private; other classes only get the collection. (composition) */
class MongoContext(uri: String, dbName: String = "sample_restaurants", val collectionName: String = "restaurants") {
  private val client: MongoClient = MongoClient(uri)
  val database: MongoDatabase = client.getDatabase(dbName)
  val collection: MongoCollection[Document] = database.getCollection(collectionName)

  def await[T](f: Future[T]): T = Await.result(f, 30.seconds)
  def close(): Unit = client.close()
}

// ======================================================================
// Repository (CRUD + search)
// ======================================================================

trait RestaurantRepository {
  def add(r: Restaurant): Unit
  def findById(id: String): Option[Restaurant]
  def searchByName(text: String, limit: Int): Seq[Restaurant]
  def searchByCuisineAndBorough(cuisine: Option[String], borough: Option[String], limit: Int): Seq[Restaurant]
  def searchByZip(zip: String, limit: Int): Seq[Restaurant]
  def searchByMinScore(minScore: Int, limit: Int): Seq[Restaurant]
  def updateField(id: String, field: String, value: String): Boolean
  def addGrade(id: String, grade: Grade): Boolean
  def delete(id: String): Boolean
  def count(): Long
}

class MongoRestaurantRepository(ctx: MongoContext) extends RestaurantRepository {

  private def coll = ctx.collection

  private def toRestaurants(docs: Seq[Document]): Seq[Restaurant] = docs.flatMap(Restaurant.fromDocument)

  private def find(filter: Bson, limit: Int): Seq[Restaurant] =
    toRestaurants(ctx.await(coll.find(filter).limit(limit).toFuture()))

  // ---------------- CREATE ----------------
  override def add(r: Restaurant): Unit = {
    if (findById(r.restaurantId).isDefined)
      throw new ValidationException(s"Restaurant id ${r.restaurantId} already exists.")
    ctx.await(coll.insertOne(r.toDocument).toFuture())
  }

  // ---------------- READ ----------------
  override def findById(id: String): Option[Restaurant] =
    find(Filters.equal("restaurant_id", id), 1).headOption

  /** Case-insensitive "contains" search on the name. */
  override def searchByName(text: String, limit: Int): Seq[Restaurant] =
    find(Filters.regex("name", Pattern.quote(text.trim), "i"), limit)

  /** Uses the compound index (borough, cuisine). Either argument may be omitted. */
  override def searchByCuisineAndBorough(cuisine: Option[String], borough: Option[String], limit: Int): Seq[Restaurant] = {
    val conditions: List[Bson] =
      cuisine.map(c => Filters.equal("cuisine", c)).toList ++
        borough.map(b => Filters.equal("borough", b)).toList
    val filter: Bson = if (conditions.isEmpty) Filters.empty() else Filters.and(conditions: _*)
    find(filter, limit)
  }

  override def searchByZip(zip: String, limit: Int): Seq[Restaurant] =
    find(Filters.equal("address.zipcode", zip.trim), limit)

  /** Restaurants that have at least one inspection score >= minScore (uses grades.score index). */
  override def searchByMinScore(minScore: Int, limit: Int): Seq[Restaurant] =
    find(Filters.gte("grades.score", minScore), limit)

  // ---------------- UPDATE ----------------
  override def updateField(id: String, field: String, value: String): Boolean = {
    val path = field match {
      case "name" | "cuisine" | "borough" => field
      case "zipcode"                      => "address.zipcode"
      case other                          => throw new ValidationException(s"Field '$other' cannot be updated.")
    }
    ctx.await(coll.updateOne(Filters.equal("restaurant_id", id), Updates.set(path, value)).toFuture()).getMatchedCount > 0
  }

  override def addGrade(id: String, grade: Grade): Boolean =
    ctx.await(
      coll
        .updateOne(
          Filters.equal("restaurant_id", id),
          // $push with $position 0 keeps "newest grade first" like the sample data
          Updates.pushEach("grades", new PushOptions().position(0), grade.toBson)
        )
        .toFuture()
    ).getMatchedCount > 0

  // ---------------- DELETE ----------------
  override def delete(id: String): Boolean =
    ctx.await(coll.deleteOne(Filters.equal("restaurant_id", id)).toFuture()).getDeletedCount > 0

  override def count(): Long = ctx.await(coll.countDocuments().toFuture())
}

// ======================================================================
// Index management
// ======================================================================

class IndexManager(ctx: MongoContext) {

  /** (name, keys, why we need it) */
  private val specs: List[(String, Bson, String)] = List(
    ("idx_restaurant_id", Indexes.ascending("restaurant_id"), "lookup / update / delete by restaurant_id"),
    ("idx_name", Indexes.ascending("name"), "restaurant name search"),
    ("idx_zipcode", Indexes.ascending("address.zipcode"), "ZIP code search"),
    ("idx_grades_score", Indexes.descending("grades.score"), "score search and score aggregations"),
    (
      "idx_borough_cuisine",
      Indexes.compoundIndex(Indexes.ascending("borough"), Indexes.ascending("cuisine")),
      "COMPOUND: cuisine + borough filtering and grouping"
    )
  )

  /** Creates every index (idempotent). Returns a status line for each. */
  def ensureIndexes(): List[String] =
    specs.map { case (name, keys, why) =>
      try {
        ctx.await(ctx.collection.createIndex(keys, IndexOptions().name(name)).toFuture())
        s"OK   $name  ->  $why"
      } catch {
        case e: Exception => s"FAIL $name  ->  ${e.getMessage}"
      }
    }

  def listIndexes(): Seq[Document] = ctx.await(ctx.collection.listIndexes[Document]().toFuture())

  /** Queries used to prove that the indexes are really used. */
  val demoQueries: List[(String, Document)] = List(
    ("Cuisine + borough (compound)", Document("borough" -> "Brooklyn", "cuisine" -> "Italian")),
    ("Name starts with 'Pizza'", Document("name" -> Document("$regex" -> "^Pizza"))),
    ("ZIP code 10001", Document("address.zipcode" -> "10001")),
    ("Inspection score >= 40", Document("grades.score" -> Document("$gte" -> 40)))
  )

  private def stages(p: BsonDocument): List[BsonDocument] =
    p :: List("inputStage", "queryPlan")
      .flatMap(k => Option(p.get(k)).filter(_.isDocument).map(_.asDocument))
      .flatMap(stages)

  /** Runs the MongoDB `explain` command and reports the winning plan. */
  def explain(filter: Document): String = {
    val cmd = Document(
      "explain"   -> Document("find" -> ctx.collectionName, "filter" -> filter),
      "verbosity" -> "queryPlanner"
    )
    val res = ctx.await(ctx.database.runCommand(cmd).toFuture()).toBsonDocument
    val winning = res.getDocument("queryPlanner").getDocument("winningPlan")
    val all = stages(winning)
    val ix = all.find(s => Option(s.get("stage")).exists(_.asString.getValue == "IXSCAN"))
    ix match {
      case Some(s) => s"IXSCAN using index '${s.getString("indexName").getValue}'"
      case None    => s"${all.headOption.flatMap(s => Option(s.get("stage"))).map(_.asString.getValue).getOrElse("?")} (no index used)"
    }
  }
}

// ======================================================================
// Aggregations
// ======================================================================

class AnalyticsService(ctx: MongoContext) {

  private def run(pipeline: Seq[Bson]): Seq[Document] = ctx.await(ctx.collection.aggregate(pipeline).toFuture())

  /** 1. Number of restaurants per cuisine (top N). */
  def restaurantsByCuisine(top: Int): Seq[Document] = run(Seq(
    Aggregates.group("$cuisine", Accumulators.sum("restaurants", 1)),
    Aggregates.sort(Sorts.descending("restaurants")),
    Aggregates.limit(top)
  ))

  /** 2. Number of restaurants per borough. */
  def restaurantsByBorough(): Seq[Document] = run(Seq(
    Aggregates.group("$borough", Accumulators.sum("restaurants", 1)),
    Aggregates.sort(Sorts.descending("restaurants"))
  ))

  /** 3. Average inspection score per cuisine (lower = cleaner). Only cuisines with enough data. */
  def avgScoreByCuisine(minInspections: Int, top: Int): Seq[Document] = run(Seq(
    Aggregates.unwind("$grades"),
    Aggregates.`match`(Filters.exists("grades.score")),
    Aggregates.group("$cuisine", Accumulators.avg("avgScore", "$grades.score"), Accumulators.sum("inspections", 1)),
    Aggregates.`match`(Filters.gte("inspections", minInspections)),
    Aggregates.sort(Sorts.ascending("avgScore")),
    Aggregates.limit(top)
  ))

  /** 4. Top-rated restaurants. In NYC inspections a LOWER score is better, so lowest average wins. */
  def topRatedRestaurants(minInspections: Int, top: Int): Seq[Document] = run(Seq(
    Aggregates.unwind("$grades"),
    Aggregates.`match`(Filters.exists("grades.score")),
    Aggregates.group(
      "$restaurant_id",
      Accumulators.first("name", "$name"),
      Accumulators.first("borough", "$borough"),
      Accumulators.first("cuisine", "$cuisine"),
      Accumulators.avg("avgScore", "$grades.score"),
      Accumulators.sum("inspections", 1)
    ),
    Aggregates.`match`(Filters.gte("inspections", minInspections)),
    Aggregates.sort(Sorts.orderBy(Sorts.ascending("avgScore"), Sorts.descending("inspections"))),
    Aggregates.limit(top)
  ))

  /** 5. Grade distribution (A, B, C, Z, P, N ...). */
  def gradeDistribution(): Seq[Document] = run(Seq(
    Aggregates.unwind("$grades"),
    Aggregates.group("$grades.grade", Accumulators.sum("count", 1)),
    Aggregates.sort(Sorts.descending("count"))
  ))
}