package vintage

import scala.reflect.runtime.universe._

object OpCodes {
  //
  // Stop and Arithmetic
  //
  val STOP = 0x00.toByte
  val ADD = 0x01.toByte
  val MUL = 0x02.toByte
  val SUB = 0x03.toByte
  val DIV = 0x04.toByte
  val SDIV = 0x05.toByte
  val MOD = 0x06.toByte
  val SMOD = 0x07.toByte
  val ADDMOD = 0x08.toByte
  val MULMOD = 0x09.toByte
  val EXP = 0x0a.toByte
  val SIGNEXTEND = 0x0b.toByte


  //
  // Comparison and Bitwise Logic
  //
  val LT = 0x10.toByte
  val GT = 0x11.toByte
  val SLT = 0x12.toByte
  val SGT = 0x13.toByte
  val EQ = 0x14.toByte
  val ISZERO = 0x15.toByte
  val AND = 0x16.toByte
  val OR = 0x17.toByte
  val XOR = 0x18.toByte
  val NOT = 0x19.toByte
  val BYTE = 0x1a.toByte


  //
  // Sha3
  //
  val SHA3 = 0x20.toByte


  //
  // Environment Information
  //
  val ADDRESS = 0x30.toByte
  val BALANCE = 0x31.toByte
  val ORIGIN = 0x32.toByte
  val CALLER = 0x33.toByte
  val CALLVALUE = 0x34.toByte
  val CALLDATALOAD = 0x35.toByte
  val CALLDATASIZE = 0x36.toByte
  val CALLDATACOPY = 0x37.toByte
  val CODESIZE = 0x38.toByte
  val CODECOPY = 0x39.toByte
  val GASPRICE = 0x3a.toByte
  val EXTCODESIZE = 0x3b.toByte
  val EXTCODECOPY = 0x3c.toByte
  val RETURNDATASIZE = 0x3d.toByte
  val RETURNDATACOPY = 0x3e.toByte


  //
  // Block Information
  //
  val BLOCKHASH = 0x40.toByte
  val COINBASE = 0x41.toByte
  val TIMESTAMP = 0x42.toByte
  val NUMBER = 0x43.toByte
  val DIFFICULTY = 0x44.toByte
  val GASLIMIT = 0x45.toByte


  //
  // Stack, Memory, Storage and Flow Operations
  //
  val POP = 0x50.toByte

  val MLOAD = 0x51.toByte
  val MSTORE = 0x52.toByte
  val MSTORE8 = 0x53.toByte
  val SLOAD = 0x54.toByte
  val SSTORE = 0x55.toByte
  val JUMP = 0x56.toByte
  val JUMPI = 0x57.toByte
  val PC = 0x58.toByte
  val MSIZE = 0x59.toByte
  val GAS = 0x5a.toByte
  val JUMPDEST = 0x5b.toByte


  //
  // Push Operations
  //
  val PUSH1 = 0x60.toByte
  val PUSH2 = 0x61.toByte
  val PUSH3 = 0x62.toByte
  val PUSH4 = 0x63.toByte
  val PUSH5 = 0x64.toByte
  val PUSH6 = 0x65.toByte
  val PUSH7 = 0x66.toByte
  val PUSH8 = 0x67.toByte
  val PUSH9 = 0x68.toByte
  val PUSH10 = 0x69.toByte
  val PUSH11 = 0x6a.toByte
  val PUSH12 = 0x6b.toByte
  val PUSH13 = 0x6c.toByte
  val PUSH14 = 0x6d.toByte
  val PUSH15 = 0x6e.toByte
  val PUSH16 = 0x6f.toByte
  val PUSH17 = 0x70.toByte
  val PUSH18 = 0x71.toByte
  val PUSH19 = 0x72.toByte
  val PUSH20 = 0x73.toByte
  val PUSH21 = 0x74.toByte
  val PUSH22 = 0x75.toByte
  val PUSH23 = 0x76.toByte
  val PUSH24 = 0x77.toByte
  val PUSH25 = 0x78.toByte
  val PUSH26 = 0x79.toByte
  val PUSH27 = 0x7a.toByte
  val PUSH28 = 0x7b.toByte
  val PUSH29 = 0x7c.toByte
  val PUSH30 = 0x7d.toByte
  val PUSH31 = 0x7e.toByte
  val PUSH32 = 0x7f.toByte


  //
  // Duplicate Operations
  //
  val DUP1 = 0x80.toByte
  val DUP2 = 0x81.toByte
  val DUP3 = 0x82.toByte
  val DUP4 = 0x83.toByte
  val DUP5 = 0x84.toByte
  val DUP6 = 0x85.toByte
  val DUP7 = 0x86.toByte
  val DUP8 = 0x87.toByte
  val DUP9 = 0x88.toByte
  val DUP10 = 0x89.toByte
  val DUP11 = 0x8a.toByte
  val DUP12 = 0x8b.toByte
  val DUP13 = 0x8c.toByte
  val DUP14 = 0x8d.toByte
  val DUP15 = 0x8e.toByte
  val DUP16 = 0x8f.toByte


  //
  // Exchange Operations
  //
  val SWAP1 = 0x90.toByte
  val SWAP2 = 0x91.toByte
  val SWAP3 = 0x92.toByte
  val SWAP4 = 0x93.toByte
  val SWAP5 = 0x94.toByte
  val SWAP6 = 0x95.toByte
  val SWAP7 = 0x96.toByte
  val SWAP8 = 0x97.toByte
  val SWAP9 = 0x98.toByte
  val SWAP10 = 0x99.toByte
  val SWAP11 = 0x9a.toByte
  val SWAP12 = 0x9b.toByte
  val SWAP13 = 0x9c.toByte
  val SWAP14 = 0x9d.toByte
  val SWAP15 = 0x9e.toByte
  val SWAP16 = 0x9f.toByte


  //
  // Logging
  //
  val LOG0 = 0xa0.toByte
  val LOG1 = 0xa1.toByte
  val LOG2 = 0xa2.toByte
  val LOG3 = 0xa3.toByte
  val LOG4 = 0xa4.toByte


  //
  // System
  //
  val CREATE = 0xf0.toByte
  val CALL = 0xf1.toByte
  val CALLCODE = 0xf2.toByte
  val RETURN = 0xf3.toByte
  val DELEGATECALL = 0xf4.toByte
  val STATICCALL = 0xfa.toByte
  val REVERT = 0xfd.toByte
  val INVALID = 0xfe.toByte
  val SELFDESTRUCT = 0xff.toByte
}

object OpCodeMaps {
  val Value2Name = {
    val rm = scala.reflect.runtime.currentMirror
    val accessors = rm.classSymbol(OpCodes.getClass).toType.members.collect {
        case m: MethodSymbol if m.isGetter && m.isPublic => m
    }
    val instanceMirror = rm.reflect(OpCodes)
    accessors.map { a => (instanceMirror.reflectMethod(a)(), a.name) }.toMap
  }

  val Name2Value = Value2Name.map(_.swap)
}
