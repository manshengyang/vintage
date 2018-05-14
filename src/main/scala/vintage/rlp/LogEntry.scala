package vintage.rlp

import vintage.{Address, UInt256}

case class LogEntry(
  address: Address,
  topics: Vector[UInt256],
  data: Array[Byte],
)

object LogEntry {
  import Implicits._

  implicit val logEntryRLP: RLPConvertible[LogEntry] = new RLPConvertible[LogEntry] {
    def convertTo(obj: LogEntry): RLPObject = RLPList(Vector[RLPObject](
      obj.address.data,
      obj.topics.map(i => wrapBytes(i.toPaddedByteArray)),
      obj.data,
    ))

    def convertFrom(rlp: RLPObject): LogEntry = {
      val l = unwrapList(rlp)
      LogEntry(
        Address(unwrapBytes(l(0))),
        unwrapList(l(1)).map(o => UInt256(unwrapBytes(o))),
        l(2),
      )
    }
  }
}
