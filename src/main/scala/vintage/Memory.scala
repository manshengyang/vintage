package vintage

import java.math.BigInteger
import scala.collection.immutable.HashMap
import vintage.UInt256Conversions._

object Memory {
  def apply() = new Memory
}

case class InvalidMemoryAccess(msg: String) extends RuntimeException(msg)

case class Memory(val m: HashMap[Word, Byte] = HashMap()) extends AnyVal {
  def load(pos: Word, len: Word): Array[Byte] = {
    checkPos(pos.v.add(len.v).subtract(BigInteger.ONE))
    (0L until len.toLong).map(i => load(pos + i)).toArray
  }

  def load(pos: Word): Byte = m.getOrElse(pos, 0)

  def store(pos: Word, bytes: Array[Byte]): Memory = {
    checkPos(pos.v.add(BigInteger.valueOf(bytes.size - 1)))
    val updates = (0 until bytes.size).map { i => (pos + i) -> bytes(i) }
    Memory(m ++ updates)
  }

  def store(pos: Word, content: Word): Memory = {
    val bytes = content.toPaddedByteArray
    store(pos, bytes)
  }

  def store(pos: Word, b: Byte): Memory = Memory(m + (pos -> b))

  private def checkPos(pos: BigInteger): Unit = {
    if (pos.compareTo(UInt256.Max.v) > 0) {
      throw InvalidMemoryAccess("Memory location exceed UInt256.Max")
    }
  }
}
