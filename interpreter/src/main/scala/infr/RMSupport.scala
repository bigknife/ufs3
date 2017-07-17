package infr

import reactivemongo.api.BSONSerializationPack.{Reader, Writer}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{MongoConnection, MongoDriver, QueryOpts}
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDocument, BSONDouble, BSONInteger, BSONLong, BSONNull, BSONObjectID, BSONString, BSONValue}
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Created by songwenchao on 2017/7/7.
  */
trait RMSupport {

  val mongoConnUri: String

  private[RMSupport] lazy val dbName = MongoConnection.parseURI(mongoConnUri).get.db.get
  private[RMSupport] lazy val driver = new MongoDriver
  private[RMSupport] lazy val connection: Future[MongoConnection] =
    Future.fromTry(MongoConnection.parseURI(mongoConnUri).map(driver.connection))

  def collection(name: String): Future[BSONCollection] =
    for {
      conn ← connection
      db   ← conn.database(dbName)
    } yield db.collection(name)

  def insertInto(collectionName: String, t: JsValue): Future[Unit] = {
    implicit val jsonWriter = RMSupport.JsonWriter
    for {
      coll ← collection(collectionName)
      _    ← coll.insert[JsValue](t)
    } yield ()
  }

  def find(collectionName: String,
           condition: BSONDocument,
           order: Option[BSONDocument] = None,
           from: Int = 0,
           to: Int = Int.MaxValue): Future[Vector[JsValue]] = {
    implicit val jsonReader = RMSupport.JsonReader
    for {
      coll ← collection(collectionName)
      vector ← if (order.isDefined) {
        coll
          .find(condition)
          .options(QueryOpts().skip(from).batchSize(to - from))
          .sort(order.get)
          .cursor[JsValue]()
          .collect[Vector](to - from)
      } else
        coll
          .find(condition)
          .options(QueryOpts().skip(from).batchSize(to - from))
          .cursor[JsValue]()
          .collect[Vector](to - from)
    } yield vector
  }

  def delete(collectionName: String, condition: BSONDocument): Future[Unit] =
    for {
      coll ← collection(collectionName)
      _    ← coll.remove(condition)
    } yield ()

  def fau(collectionName: String, condition: BSONDocument, t: JsValue, upsert: Boolean = false): Future[JsValue] = {
    implicit val jsonReader = RMSupport.JsonReader
    implicit val jsonWriter = RMSupport.JsonWriter
    for {
      coll ← collection(collectionName)
      rt ← coll.findAndUpdate[BSONDocument, BSONDocument](
        selector = condition,
        update = BSONDocument("$set" → jsonWriter.write(t)),
        fetchNewObject = true,
        upsert = upsert
      )
      result ← {
        rt.result[JsValue] match {
          case Some(value) ⇒ Future.successful(value)
          case None ⇒
            Future.failed(
              new IllegalArgumentException(s"nothing found and update $collectionName for condition: $condition"))
        }
      }
    } yield result
  }

  def countBy(collectionName: String, condition: BSONDocument): Future[Int] =
    for {
      coll  ← collection(collectionName)
      count ← coll.count(Some(condition))
    } yield count
}

object RMSupport {

  implicit object JsonReader extends Reader[JsValue] {

    def bsonValue2JsonValue(bsonValue: BSONValue): JsValue = bsonValue match {
      case BSONString(x)  ⇒ JsString(x)
      case BSONLong(x)    ⇒ JsNumber(x)
      case BSONInteger(x) ⇒ JsNumber(x.toLong)
      case BSONDouble(x)  ⇒ JsNumber(x)
      case BSONBoolean(x) ⇒ JsBoolean(x)
      case BSONArray(stream) ⇒
        JsArray(
          stream.foldLeft(Vector.empty[JsValue]) { (acc, n) ⇒
            n match {
              case Success(x) ⇒ acc :+ bsonValue2JsonValue(x)
              case Failure(e) ⇒ throw e
            }
          }
        )
      case BSONDocument(stream) ⇒
        JsObject(
          stream.foldLeft(Map.empty[String, JsValue]) { (acc, n) ⇒
            n match {
              case Success((key, value)) ⇒ acc + (key → bsonValue2JsonValue(value))
              case Failure(e)            ⇒ throw e
            }
          }
        )
      case BSONNull ⇒ JsNull
      case x        ⇒ throw new IllegalArgumentException(s"$x can't be written to json value")
    }

    override def read(bson: BSONDocument): JsValue = bson.elements.foldLeft(JsObject()) { (acc, n) ⇒
      n match {
        case ("_id", x: BSONObjectID) ⇒ JsObject(acc.fields + ("_id" → JsString(x.stringify)))
        case (key, BSONInteger(x))    ⇒ JsObject(acc.fields + (key   → JsNumber(x.toLong)))
        case (key, BSONLong(x))       ⇒ JsObject(acc.fields + (key   → JsNumber(x)))
        case (key, BSONDouble(x))     ⇒ JsObject(acc.fields + (key   → JsNumber(x)))
        case (key, BSONString(x))     ⇒ JsObject(acc.fields + (key   → JsString(x)))
        case (key, BSONBoolean(x))    ⇒ JsObject(acc.fields + (key   → JsBoolean(x)))
        case (key, BSONArray(stream)) ⇒
          JsObject(
            acc.fields + (key → JsArray(stream.foldLeft(Vector.empty[JsValue]) { (acc, n) ⇒
              n match {
                case Success(x) ⇒ acc :+ bsonValue2JsonValue(x)
                case Failure(e) ⇒ throw e
              }
            }))
          )
        case (key, document @ BSONDocument(_)) ⇒ JsObject(acc.fields + (key → read(document)))
        case _                                 ⇒ acc
      }
    }
  }

  implicit object JsonWriter extends Writer[JsValue] {

    def jsonValue2BsonValue(json: JsValue): BSONValue = json match {
      case JsString(x)                  ⇒ BSONString(x)
      case JsNumber(x) if x.isValidLong ⇒ BSONLong(x.toLong)
      case JsNumber(x)                  ⇒ BSONDouble(x.toDouble)
      case JsBoolean(x)                 ⇒ BSONBoolean(x)
      case JsArray(x)                   ⇒ BSONArray(x map jsonValue2BsonValue)
      case JsObject(x)                  ⇒ BSONDocument(x.map(m ⇒ m._1 → jsonValue2BsonValue(m._2)))
      case JsNull                       ⇒ BSONNull
      case x                            ⇒ throw new IllegalArgumentException(s"$x can't transform to bson value")
    }

    override def write(t: JsValue): BSONDocument = t match {
      case JsObject(fields) ⇒
        fields.foldLeft(BSONDocument.empty) { (acc, n) ⇒
          n match {
            case ("_id", jsv @ JsString(_id)) ⇒
              BSONObjectID.parse(_id) match {
                case Success(x) ⇒ acc ++ BSONDocument("_id" → x)
                case Failure(_) ⇒ acc ++ BSONDocument("_id" → jsonValue2BsonValue(jsv))
              }
              acc
            case (k, jsv) ⇒ acc ++ BSONDocument(k → jsonValue2BsonValue(jsv))
          }
        }
      case _ ⇒ throw new IllegalArgumentException("only json object can be written as a bson value")
    }
  }

}
