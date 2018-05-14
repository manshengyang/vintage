import com.google.common.io.BaseEncoding

import vintage.rlp.Account

package object vintage {
  type Word = UInt256
  val WordSize = 32

  val Hex = BaseEncoding.base16().lowerCase()

  type SystemState = Map[Address, Account]
}
