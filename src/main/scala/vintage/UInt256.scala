package vintage

import java.math.BigInteger

case class UInt256Overflow() extends RuntimeException("UInt256 overflow")

object UInt256 {
  val ByteSize = 32
  val Zero = new UInt256(BigInteger.ZERO)
  val One = new UInt256(BigInteger.ONE)
  val Base = BigInteger.valueOf(2).pow(256)
  val Max = new UInt256(Base.subtract(BigInteger.ONE))

  def apply(v: BigInteger, check: Boolean = false): UInt256 = {
    if (check && v.compareTo(Max.v) > 0) {
      throw UInt256Overflow()
    }
    new UInt256(v.and(Max.v))
  }
  def apply(v: Long): UInt256 = apply(BigInteger.valueOf(v))
  def apply(v: Array[Byte]): UInt256 = apply(new BigInteger(1, v)) // always positive
}

object UInt256Conversions {
  implicit def boolToUInt256(b: Boolean) = if (b) UInt256.One else UInt256.Zero
  implicit def longToUInt256(i: Long) = UInt256(i)
  implicit def bytesToUInt256(bytes: Array[Byte]) = UInt256(bytes)
  implicit def bigIntegerToUInt256(i: BigInteger) = UInt256(i)
}

class UInt256(val v: BigInteger) extends AnyVal {
  import UInt256._

  def +(other: UInt256, check: Boolean = false) = UInt256(v.add(other.v), check)
  def -(other: UInt256) = UInt256(v.subtract(other.v))
  def *(other: UInt256, check: Boolean = false) = UInt256(v.multiply(other.v), check)
  def /(other: UInt256) = if (other == Zero) Zero else UInt256(v.divide(other.v))

  def addChecked(other: UInt256) = this.+(other, true)
  def mulChecked(other: UInt256) = this.*(other, true)

  def sdiv(other: UInt256) =
    if (other == Zero) Zero else UInt256(toSigned.divide(other.toSigned))

  def %(other: UInt256) = if (other == Zero) Zero else new UInt256(v.mod(other.v))

  def smod(other: UInt256) = {
    if (other == Zero) {
      Zero
    } else {
      val signed = toSigned
      UInt256(signed.abs.mod(other.toSigned.abs).multiply(BigInteger.valueOf(signed.signum)))
    }
  }

  def addMod(other: UInt256, m: UInt256) = {
    if (m == Zero) Zero
    else UInt256(v.add(other.v).mod(m.v))
  }
  def mulMod(other: UInt256, m: UInt256) = {
    if (m == Zero) Zero
    else UInt256(v.multiply(other.v).mod(m.v))
  }

  def pow(e: UInt256) = new UInt256(v.modPow(e.v, Base))

  def &(other: UInt256) = new UInt256(v.and(other.v))
  def |(other: UInt256) = new UInt256(v.or(other.v))
  def ^(other: UInt256) = new UInt256(v.xor(other.v))
  def unary_~() = UInt256(v.not)

  def >(other: UInt256): Boolean = v.compareTo(other.v) > 0
  def >=(other: UInt256): Boolean = v.compareTo(other.v) >= 0
  def <(other: UInt256): Boolean = v.compareTo(other.v) < 0
  def <=(other: UInt256): Boolean = v.compareTo(other.v) <= 0
  def ==(other: UInt256): Boolean = v == other.v
  def !=(other: UInt256): Boolean = v != other.v

  def sgt(other: UInt256): Boolean = toSigned.compareTo(other.toSigned) > 0
  def slt(other: UInt256): Boolean = toSigned.compareTo(other.toSigned) < 0

  override def toString(): String = v.toString

  def toPaddedByteArray: Array[Byte] = {
    val bytes = v.toByteArray
    if (bytes.size > ByteSize) { // BigInteger.toByteArray may contains an extra sign bit
      bytes.drop(1)
    } else {
      new Array[Byte](32 - bytes.size) ++ bytes
    }
  }

  def toByteArray: Array[Byte] = {
    val ret = v.toByteArray
    if (ret(0) == 0) {
      ret.drop(1)
    } else {
      ret
    }
  }

  def toLong: Long = v.longValue
  def toInt: Int = v.intValue
  def toByte: Byte = v.byteValue

  def toSigned: BigInteger = new BigInteger(toPaddedByteArray)

  def byteLength: Int = (v.bitLength - 1) / 8 + 1
}
