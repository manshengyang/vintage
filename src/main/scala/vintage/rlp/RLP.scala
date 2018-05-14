package vintage.rlp

import vintage.UInt256

case class RLPException(msg: String) extends RuntimeException(msg)

sealed trait RLPObject extends Any
case class RLPBytes(val bytes: ByteArray) extends AnyVal with RLPObject
case class RLPList(val list: Vector[RLPObject] = Vector()) extends AnyVal with RLPObject

trait RLPConvertible[A] {
  def convertTo(obj: A): RLPObject
  def convertFrom(rlp: RLPObject): A
}

object Implicits {
  implicit val stringRLP: RLPConvertible[String] = new RLPConvertible[String] {
    def convertTo(obj: String): RLPObject = RLPBytes(obj.getBytes)
    def convertFrom(rlp: RLPObject): String = rlp match {
      case RLPBytes(bytes) => new String(bytes)
      case _ => throw new RLPException("Invalid string rlp")
    }
  }

  implicit val bytesRLP: RLPConvertible[ByteArray] = new RLPConvertible[ByteArray] {
    def convertTo(obj: ByteArray): RLPObject = RLPBytes(obj)
    def convertFrom(rlp: RLPObject): ByteArray = rlp match {
      case RLPBytes(bytes) => bytes
      case _ => throw new RLPException("Invalid byte array rlp")
    }
  }

  implicit val uint256RLP: RLPConvertible[UInt256] = new RLPConvertible[UInt256] {
    def convertTo(n: UInt256): RLPObject = RLPBytes(RLPCodec.toBytes(n))
    def convertFrom(rlp: RLPObject): UInt256 = RLPCodec.toScalar(unwrapBytes(rlp))
  }

  implicit def wrapBytes(bytes: ByteArray): RLPObject = bytesRLP.convertTo(bytes)
  implicit def unwrapBytes(obj: RLPObject): ByteArray = bytesRLP.convertFrom(obj)

  implicit def wrapList(l: Vector[RLPObject]): RLPObject = RLPList(l)
  implicit def unwrapList(obj: RLPObject): Vector[RLPObject] = obj match {
    case RLPList(l) => l
    case _ => ???
  }

  implicit def autoWrap[A: RLPConvertible](a: A): RLPObject =
    implicitly[RLPConvertible[A]].convertTo(a)
  implicit def autoUnwrap[A: RLPConvertible](rlp: RLPObject): A =
    implicitly[RLPConvertible[A]].convertFrom(rlp)
}

object RLPCodec {
  val BytesType = 0
  val ListType = 1

  def wrap(b: Byte): ByteArray = Array(b)

  def encodeAuto[A](obj: A)(implicit c: RLPConvertible[A]): ByteArray =
    encode(c.convertTo(obj))

  def encodeAuto[A](xs: Seq[A])(implicit c: RLPConvertible[A]): ByteArray =
    encode(RLPList(xs.map(x => c.convertTo(x)).toVector))

  def decodeAuto[A](input: ByteArray)(implicit c: RLPConvertible[A]): A =
    c.convertFrom(decode(input))

  def encode(obj: RLPObject): ByteArray = {
    obj match {
      case RLPBytes(bytes) => {
        encode(bytes)
      }
      case RLPList(list) => {
        encode(list.map(encode _))
      }
    }
  }

  def encode(bytes: ByteArray): ByteArray = {
    if (bytes.size == 1 && java.lang.Byte.toUnsignedInt(bytes(0)) < 0x80) bytes
    else encodeLength(bytes.size, 0x80) ++ bytes
  }

  def encode(objs: Seq[ByteArray]): ByteArray = {
    val size = if (objs.isEmpty) 0 else objs.map(_.size).reduce(_ + _)
    encodeLength(size, 0xc0) ++ objs.flatten[Byte]
  }

  def decode(input: ByteArray): RLPObject = {
    val l = decodeList(input)
    if (l.size == 1) {
      l(0)
    } else {
      throw RLPException(s"Invalid input: ${l.size} objects when one is expected")
    }
  }

  def decodeList(input: ByteArray): List[RLPObject] = {
    if (input.isEmpty) {
      List()
    } else {
      val (offset, dataLen, t) = decodeLength(input)
      val output: RLPObject = if (t == BytesType) {
        val bytes = input.slice(offset, offset + dataLen)
        if (offset > 0 && bytes.size == 1 && java.lang.Byte.toUnsignedInt(bytes(0)) < 0x80) {
          throw RLPException("Invalid encoded data for single byte")
        }
        RLPBytes(bytes)
      } else if (t == ListType) {
        RLPList(decodeList(input.slice(offset, offset + dataLen)).toVector)
      } else {
        ???
      }
      if (offset + dataLen <= 0) {
        throw RLPException(s"Invalid length: $offset, $dataLen")
      }
      val rest = decodeList(input.drop(offset + dataLen))
      output::rest
    }
  }

  private def encodeLength(len: Int, offset: Int): ByteArray = {
    if (len < 56) {
      wrap((len + offset).toByte)
    } else {
      val lenBytes = toBytes(UInt256(len))
      wrap((lenBytes.size + offset + 55).toByte) ++ lenBytes
    }
  }

  private def decodeLength(input: ByteArray): (Int, Int, Int) = {
    val len = input.size
    if (len == 0) {
      throw RLPException("Invalid length: empty")
    }
    val prefix = java.lang.Byte.toUnsignedInt(input(0))
    if (prefix <= 0x7f) {
      (0, 1, BytesType)
    } else if (prefix <= 0xb7 && len > prefix - 0x80) {
      (1, prefix - 0x80, BytesType)
    } else if (prefix <= 0xbf && len > prefix - 0xb7) {
      val sizeLen = prefix - 0xb7
      val size = toScalar(input.slice(1, 1 + sizeLen)).toInt
      if (size >= 56 && len > sizeLen + size) {
        (1 + sizeLen, size, BytesType)
      } else {
        throw RLPException("Invalid length")
      }
    } else if (prefix <= 0xf7 && len > prefix - 0xc0) {
      (1, prefix - 0xc0, ListType)
    } else if (prefix <= 0xff && len > prefix - 0xf7) {
      val sizeLen = prefix - 0xf7
      val size = toScalar(input.slice(1, 1 + sizeLen)).toInt
      if (size >= 56 && len > sizeLen + size) {
        (1 + sizeLen, size, ListType)
      } else {
        throw RLPException(s"Invalid length: $sizeLen + $size >= $len")
      }
    } else {
      throw RLPException("Invalid length")
    }
  }

  def toBytes(size: UInt256): ByteArray = {
    if (size == UInt256.Zero) {
      Array()
    } else {
      size.toByteArray
    }
  }

  def toScalar(bytes: ByteArray): UInt256 = UInt256(bytes)
}

