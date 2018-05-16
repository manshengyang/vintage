package vintage

object Address {
  val Size = 20 // 20 bytes

  def apply(data: Array[Byte]) = if (data.size == Size || data.isEmpty) {
    new Address(Hex.encode(data))
  } else {
    throw new IllegalArgumentException(s"Invalid address data ${Hex.encode(data)}")
  }
  def apply() = None
  def apply(s: String) = if (s.startsWith("0x") && s.size == 42) {
    new Address(s.substring(2))
  } else if (s.size == 40) {
    new  Address(s)
  } else {
    throw new IllegalArgumentException(s"Invalid address string $s")
  }
  def apply(n: UInt256): Address = fromHash(n.toPaddedByteArray)
  def fromHash(data: Array[Byte]) = apply(data.drop(WordSize - Size))

  val None = new Address("")
}

class Address(val value: String) extends AnyVal {
  def data(): Array[Byte] = Hex.decode(value)

  def toUInt256: UInt256 = UInt256(data())

  override def toString(): String = "0x" + value

  def isEmpty: Boolean = value.isEmpty
}
