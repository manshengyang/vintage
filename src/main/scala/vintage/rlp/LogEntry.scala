package vintage.rlp

import vintage.{Address, UInt256}

case class LogEntry(
  address: Address,
  topics: Vector[Array[Byte]],
  data: Array[Byte],
)

object LogEntry {
  import Implicits._

  implicit val logEntryRLP: RLPConvertible[LogEntry] = new RLPConvertible[LogEntry] {
    def convertTo(obj: LogEntry): RLPObject = RLPList(Vector[RLPObject](
      obj.address,
      obj.topics.map(i => wrapBytes(i)),
      obj.data,
    ))

    def convertFrom(rlp: RLPObject): LogEntry = {
      val l = unwrapList(rlp)
      LogEntry(
        autoUnwrap[Address](l(0)),
        unwrapList(l(1)).map(o => unwrapBytes(o)),
        l(2),
      )
    }
  }
}
