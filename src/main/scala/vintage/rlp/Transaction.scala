package vintage.rlp

import vintage._
import vintage.UInt256Conversions._

case class Transaction(
  nonce: UInt256 = UInt256.Zero, // n
  gasPrice: UInt256 = UInt256.Zero, // p
  gas: UInt256 = UInt256.Zero, // g
  to: Address = Address(), // t
  value: UInt256 = UInt256.Zero, // v
  data: Array[Byte] = Array(), // d, i
  v: UInt256 = UInt256.Zero, // w
  r: UInt256 = UInt256.Zero, // r
  s: UInt256 = UInt256.Zero, // s
) {
  import Implicits._

  def getUnsigned(): RLPObject = {
    val w = v.toInt
    if (w == 27 || w == 28) {
      RLPList(Vector[RLPObject](nonce, gasPrice, gas, to, value, data))
    } else {
      val chainId = UInt256((w - 35) / 2)
      RLPList(Vector[RLPObject](
        nonce, gasPrice, gas, to, value, data, chainId, Array[Byte](), Array[Byte]()))
    }
  }

  def recoverSender(): Address = {
    val w = v.toInt
    val rlp = getUnsigned()
    val (v0) = if (w == 27 || w == 28) {
      w
    } else {
      28 - (w % 2)
    }
    val hash = Kec.hash(RLPCodec.encode(rlp))
    val key = ECKey.recoverFromSignature(v0 - 27, new ECKey.ECDSASignature(r.v, s.v), hash, false)
    Address(key.getAddress())
  }
}

object Transaction {
  import Implicits._

  implicit val transactionRLP: RLPConvertible[Transaction] = new RLPConvertible[Transaction] {
    def convertTo(obj: Transaction): RLPObject = RLPList(Vector[RLPObject](
      obj.nonce,
      obj.gasPrice,
      obj.gas,
      obj.to,
      obj.value,
      obj.data,
      obj.v,
      obj.r,
      obj.s
    ))

    def convertFrom(rlp: RLPObject): Transaction = {
      val l = unwrapList(rlp)
      Transaction(
        autoUnwrap[UInt256](l(0)),
        autoUnwrap[UInt256](l(1)),
        autoUnwrap[UInt256](l(2)),
        autoUnwrap[Address](l(3)),
        autoUnwrap[UInt256](l(4)),
        unwrapBytes(l(5)),
        autoUnwrap[UInt256](l(6)),
        autoUnwrap[UInt256](l(7)),
        autoUnwrap[UInt256](l(8)),
      )
    }
  }
}
