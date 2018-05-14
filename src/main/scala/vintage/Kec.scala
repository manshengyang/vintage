package vintage

import fr.cryptohash.Keccak256

object Kec {
  val EmptyHash = Hex.encode(hash(Array()))

  def hash(input: Array[Byte]): Array[Byte] = new Keccak256().digest(input)
  def addressHash(input: Array[Byte]) = hash(input).drop(12)
}
