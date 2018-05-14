package vintage

object Address {
  val Size = 20 // 20 bytes

  def apply(data: Array[Byte]) = if (data.size == Size) {
    new Address("0x" + Hex.encode(data))
  } else {
    throw new IllegalArgumentException("Invalid address data")
  }
  def apply() = new Address("0x")
  def apply(s: String) = if (s.startsWith("0x") && s.size == 42) {
    new Address(s)
  } else {
    throw new IllegalArgumentException("Invalid address string")
  }
  def apply(n: UInt256): Address = fromHash(n.toPaddedByteArray)
  def fromHash(data: Array[Byte]) = apply(data.drop(WordSize - Size))

  val None = new Address("")
}

class Address(val value: String) extends AnyVal {
  def data(): Array[Byte] = Hex.decode(value.substring(2))
  def toUInt256: UInt256 = UInt256(data())
  override def toString(): String = value
}
