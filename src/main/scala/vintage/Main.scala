package vintage

import vintage.UInt256Conversions._

object Main {
  def main(args: Array[String]): Unit = {
    val code = Hex.decode(args(0))
    val data = if (args.size > 1) {
      Hex.decode(args(1))
    } else {
      Array[Byte]()
    }

    println(s"size: ${code.size}")
    val db = new HashMapDb()
    val header = BlockHeader(
      Array(),
      Array(),
      Address(),
      Array(),
      Array(),
      Array(),
      1,
      1,
      10000,
      100,
      1,
      Array(),
      Array(),
      1,
    )
    val blockContext = new BlockContext {
      def getBlockHash(h: BlockHeader, d: UInt256) = ???
      def getCurrentHeader() = header
    }
    val vm = new VM(db, blockContext)
    val i = ExecutionEnv(
      Address(),
      Address(),
      1,
      data,
      Address(),
      1000,
      code,
      0,
      true,
    )
    val ExecutionResult(theta2, gas, a, o) = vm.executeCode(Map(), 1000, i)
    if (o != null && !o.isEmpty) {
      println(s"OUT: 0x${Hex.encode(o)}")
    }
  }
}
