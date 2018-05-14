package vintage.rlp

import vintage.{Address, Kec, UInt256}

case class Account(
  nonce: UInt256 = UInt256.Zero,
  balance: UInt256 = UInt256.Zero,
  storageRoot: String = "",
  codeHash: String = Kec.EmptyHash,
) {
  def isEmpty: Boolean =
    nonce == UInt256.Zero && balance == UInt256.Zero && codeHash == Kec.EmptyHash
}

object Account {
  import Implicits._

  implicit val accountRLP: RLPConvertible[Account] = new RLPConvertible[Account] {
    def convertTo(obj: Account): RLPObject = RLPList(Vector[RLPObject](
      obj.nonce,
      obj.balance,
      obj.storageRoot,
      obj.codeHash,
    ))

    def convertFrom(rlp: RLPObject): Account = {
      val l = unwrapList(rlp)
      Account(
        autoUnwrap[UInt256](l(0)),
        autoUnwrap[UInt256](l(1)),
        autoUnwrap[String](l(2)),
        autoUnwrap[String](l(3)),
      )
    }
  }

  def generateAddress(sender: Address, nonce: UInt256): Address = {
    val rlp = RLPCodec.encode(RLPList(Vector[RLPObject](sender.data(), RLPCodec.toBytes(nonce))))
    val hash = Kec.hash(rlp)
    Address.fromHash(hash)
  }
}
