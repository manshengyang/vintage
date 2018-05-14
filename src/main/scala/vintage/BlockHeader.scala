package vintage

case class BlockHeader(
  parentHash: Array[Byte], // p
  ommersHash: Array[Byte], // o
  benificiary: Address, // c
  stateRoot: Array[Byte], // r
  transactionRoot: Array[Byte], // t
  logsBloom: Array[Byte], // b
  difficulty: Long, // d
  number: Long, // i
  gasLimit: Long, // l
  gasUsed: Long, // g
  timestamp: Int, // s
  extraData: Array[Byte], //x
  mixHash: Array[Byte], // m
  nonce: Long, // n
)
