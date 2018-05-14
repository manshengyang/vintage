package vintage

import java.util.{HashMap => JHashMap}
import scala.collection.mutable.{HashMap => SHashMap}

object Db {
  type Key = String
  type Value = Array[Byte]
}

trait Db {
  import Db._

  def get(key: Key): Value
  def put(key: Key, value: Value): Unit

  def get(key: Array[Byte]): Value = get(Hex.encode(key))
  def put(key: Array[Byte], value: Value): Unit = put(Hex.encode(key), value)
  def put(value: Value): Key = {
    val k = Hex.encode(Kec.hash(value))
    put(k, value)
    k
  }
}

case class HashMapDb(val map: JHashMap[Db.Key, Db.Value] = new JHashMap()) extends Db {
  import Db._

  def get(key: Key): Value = map.get(key)
  def put(key: Key, value: Value): Unit = map.put(key, value)
}

case class JournalDb(
    val underlying: Db, val changes: SHashMap[Db.Key, Db.Value] = SHashMap())
    extends Db {
  import Db._

  def get(key: Key): Value = changes.getOrElse(key, underlying.get(key))
  def put(key: Key, value: Value): Unit = changes += (key -> value)
  def commit(): Unit = {
    changes.foreach { case (k, v) => underlying.put(k, v) }
  }
}
