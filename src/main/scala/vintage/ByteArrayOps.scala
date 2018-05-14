package vintage

import UInt256Conversions._

object ByteArrayOps {
  implicit class RichByteArray(val bytes: Array[Byte]) extends AnyVal {
    def read(i: Int): Byte = if (i < bytes.size) bytes(i) else 0
    def read(i: Int, n: Int): Array[Byte] = (i until (i + n)).map(read).toArray

    private def readOffset(i: Word, offset: Int): Byte =
      if (offset < bytes.size && i < bytes.size - offset) bytes((i + offset).toInt) else 0
    def read(i: Word, n: Word): Array[Byte] = {
      if (n > Int.MaxValue) throw new IllegalArgumentException("Invalid size")
      (0 until n.toInt).map(d => readOffset(i, d)).toArray
    }
  }
}
