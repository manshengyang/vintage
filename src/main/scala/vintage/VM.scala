package vintage

import scala.annotation.tailrec
import java.math.BigInteger
import vintage.UInt256Conversions._
import vintage.ByteArrayOps._
import vintage.rlp.{Account, LogEntry}

// I
case class ExecutionEnv(
  // a
  address: Address,
  // o
  origin: Address,
  // p
  gasPrice: UInt256,
  // d
  data: Array[Byte],
  // s
  caller: Address,
  // v
  value: UInt256,
  // b
  code: Array[Byte],
  // e
  depth: Int,
  // w
  perm: Boolean,
)

case class MachineState(
  // g
  gas: UInt256,
  // pc
  pc: Int = 0,
  // m
  memory: Memory = Memory(),
  // i
  activeMemory: UInt256 = 0,
  // s
  stack: Stack = Stack(),
  // o
  output: Array[Byte] = Array(),
)

case class Substate(
  // s
  suisides: Vector[Address],
  // l
  logs: Vector[LogEntry],
  // t
  touchedAccounts: Vector[Address],
) {
  def merge(other: Substate): Substate =
    Substate(
      suisides ++ other.suisides,
      logs ++ other.logs,
      touchedAccounts ++ other.touchedAccounts)
}

object Substate {
  val Empty = Substate(Vector(), Vector(), Vector())
}

trait BlockContext {
  def getBlockHash(current: BlockHeader, depth: UInt256): Array[Byte]
  def getCurrentHeader(): BlockHeader
}

case class ExecutionResult(
    theta: SystemState,
    gasLeft: UInt256,
    substate: Substate,
    output: Array[Byte]) {
  def hasError: Boolean = theta == null
  def burnAllGas: Boolean = hasError && output != null
}

object VM {
  class ExecResultBuilder(
    var theta: SystemState,
    var a: Substate,
    var i: ExecutionEnv,
    var gas: UInt256,
    var pc: Int,
    var memory: Memory,
    var activeMemory: UInt256,
    var stack: Stack,
    var lastOutput: Array[Byte],
    var output: Array[Byte],
    var cost: UInt256,
    val prevActiveMemory: UInt256,
  ) {
    def this(theta: SystemState, u: MachineState, a: Substate, i: ExecutionEnv) =
      this(theta, a, i, u.gas, u.pc, u.memory, u.activeMemory, u.stack, u.output, null, 0,
        u.activeMemory)

    def build(): (SystemState, MachineState, Substate, ExecutionEnv, Array[Byte]) =
      (theta, MachineState(gas, pc, memory, activeMemory, stack, lastOutput), a, i, output)

    def setTheta(v: SystemState) = {
      theta = v
      this
    }
    def setI(v: ExecutionEnv) = {
      i = v
      this
    }

    def setSubstate(v: Substate) = {
      a = v
      this
    }
    def addLog(l: LogEntry) = {
      a = a.copy(logs = a.logs :+ l)
      this
    }

    def setGas(v: UInt256) = {
      gas = v
      this
    }
    def setPc(v: Int) = {
      pc = v
      this
    }
    def setMemory(v: Memory) = {
      memory = v
      this
    }
    def setActiveMemory(v: UInt256) = {
      if (v > activeMemory) activeMemory = v
      this
    }
    def setStack(v: Stack) = {
      stack = v
      this
    }
    def setLastOutput(v: Array[Byte]) = {
      lastOutput = v
      this
    }
    def setOutput(v: Array[Byte]) = {
      output = v
      this
    }
    def setCost(v: UInt256) = {
      cost = v
      this
    }

    def setActiveMemoryBytes(pos: UInt256) = {
      // this is equivalent to ceil((pos + 1) / WordSize)
      setActiveMemory(MathUtils.divCeiling(new UInt256(pos.v.add(UInt256(1).v)), WordSize))
    }

    def setActiveMemoryBytes(pos: UInt256, size: UInt256): ExecResultBuilder =
      if (size == UInt256.Zero) this
      else setActiveMemoryBytes(pos.addChecked(size - 1))

    def getTotalCost(): UInt256 =
      cost + getMemoryCost(activeMemory) - getMemoryCost(prevActiveMemory)

    def burnGas() = {
      val totalCost = getTotalCost()
      if (totalCost > gas) {
        throw ExecutionException("Insufficient gas")
      }
      gas -= totalCost
      this
    }

    def refundGas(g: UInt256) = {
      gas += g
      this
    }
  }

  case class AuxInfo(
    val jumpDests: Set[Int]
  )

  def getMemoryCost(a: UInt256) = {
    val c1 = a.mulChecked(Fee.GAS_MEMORY)
    val c2 = UInt256(
      a.v.multiply(a.v).divide(BigInteger.valueOf(Fee.GAS_MEMORY_QUADRATIC_DENOMINATOR)),
      true)
    c1.addChecked(c2)
  }

  def getCopyCost(size: UInt256, base: Int) = {
    MathUtils.divCeiling(size, UInt256(32)).mulChecked(Fee.GAS_COPY).addChecked(base)
  }

  def getNextInstruction(w: Int, cur: Int): Int = {
    if (w >= OpCodes.PUSH1 && w <= OpCodes.PUSH32) {
      cur + w - OpCodes.PUSH1 + 2
    } else {
      cur + 1
    }
  }

  @tailrec
  final def getJumpDestinations(code: Array[Byte], i: Int = 0, acc: List[Int] = List())
      : List[Int] = {
    if (i >= code.size) acc
    else if (code(i) == OpCodes.JUMPDEST) getJumpDestinations(code, i + 1, i::acc)
    else getJumpDestinations(code, getNextInstruction(code(i), i), acc)
  }

  def isDeadAccount(theta: SystemState, addr: Address): Boolean = {
    theta.get(addr) match {
      case Some(acc) => acc.isEmpty
      case _ => true
    }
  }
}

class VM(db: Db, blockContext: BlockContext) extends Logging {
  import VM._

  def execute(theta: SystemState, gas: UInt256, i: ExecutionEnv, r: Address): ExecutionResult = {
    // TODO: precompiled
    executeCode(theta, gas, i)
  }

  def callMessage(
      theta: SystemState, sender: Address, origin: Address, recipient: Address,
      codeAccount: Address, gas: UInt256, gasPrice: UInt256, transferValue: UInt256,
      value: UInt256, data: Array[Byte], depth: Int, perm: Boolean): ExecutionResult = {
    val (code, theta1) = {
      val t1 = theta.get(sender) match {
        case Some(acc) => theta + (sender -> acc.copy(acc.balance - transferValue.toLong))
        case None => theta
      }
      val t2 = theta.get(recipient) match {
        case Some(acc) => t1 + (recipient -> acc.copy(acc.balance + transferValue.toLong))
        case None if transferValue > 0 => t1 + (recipient -> Account(balance = transferValue.toLong))
        case _ => t1
      }
      val code = theta.get(codeAccount) match {
        case Some(acc) => db.get(acc.codeHash)
        case _ => Array[Byte]()
      }
      (code, t2)
    }
    val i = ExecutionEnv(recipient, origin, gasPrice, data, sender, value, code, depth, perm)
    val ret = execute(theta1, gas, i, recipient)
    val t = if (ret.hasError) null else ret.theta
    val g = if (ret.burnAllGas) UInt256(0) else ret.gasLeft
    ExecutionResult(t, g, ret.substate, ret.output)
  }

  def create(
      theta: SystemState, sender: Address, origin: Address, creationAddr: Address,
      gas: UInt256, gasPrice: UInt256, value: UInt256, code: Array[Byte], depth: Int,
      perm: Boolean): ExecutionResult = {
    val senderAcc = theta(sender)
    val newSenderAcc = senderAcc.copy(
      nonce = senderAcc.nonce + 1, balance = senderAcc.balance - value)
    val newAcc = {
      val b = theta.get(creationAddr) match {
        case Some(a) => a.balance
        case _ => UInt256.Zero
      }
      Account(1, value + b)
    }
    val theta1 = theta ++ Seq(sender -> newSenderAcc, creationAddr -> newAcc)

    val i = ExecutionEnv(creationAddr, origin, gasPrice, Array(), sender, value, code, depth, perm)
    val ret = execute(theta1, gas, i, creationAddr)

    val deposit = Fee.GAS_CODEDEPOSIT * code.size
    if (ret.hasError || ret.gasLeft < deposit || code.size > 24576) {
      ExecutionResult(null, 0, Substate.Empty, null)
    } else {
      val theta2 = if (isDeadAccount(ret.theta, creationAddr)) {
        ret.theta - creationAddr
      } else {
        val acc = ret.theta(creationAddr)
        ret.theta + (creationAddr -> acc.copy(codeHash = db.put(code)))
      }
      ExecutionResult(theta2, ret.gasLeft - deposit, ret.substate, Array())
    }
  }

  def executeCode(theta: SystemState, gas: UInt256, i: ExecutionEnv): ExecutionResult = {
    val init = MachineState(gas)
    val aux = AuxInfo(getJumpDestinations(i.code).toSet)
    val (t, u, a, _, o) = executeRec(theta, init, Substate.Empty, i, aux)
    if (t != null) {
      ExecutionResult(t -- a.suisides, u.gas, a, o)
    } else {
      ExecutionResult(t, u.gas, a, o)
    }
  }

  // X
  @tailrec
  final def executeRec(
      theta: SystemState, u: MachineState, a: Substate, i: ExecutionEnv, aux: AuxInfo)
      : (SystemState, MachineState, Substate, ExecutionEnv, Array[Byte]) = {
    var error = false
    val w = i.code.read(u.pc)
    val (theta2, u2, a2, i2, o) = try {
      executeCycle(theta, u, a, i, aux)
    } catch {
      case e: RuntimeException =>
        log.debug(e.getMessage, e)
        error = true
        (null, u, Substate.Empty, i, null)
    }
    if (error || o != null) {
      // halt
      (theta2, u2, a2, i2, o)
    } else if (w == OpCodes.REVERT) {
      (null, u2, Substate.Empty, i, o)
    } else {
      executeRec(theta2, u2, a2, i2, aux)
    }
  }

  // O
  final def executeCycle(
      theta: SystemState, u: MachineState, a: Substate, i: ExecutionEnv, aux: AuxInfo)
      : (SystemState, MachineState, Substate, ExecutionEnv, Array[Byte]) = {
    val w = i.code.read(u.pc)

    val pc = u.pc
    val stack = u.stack
    val mem = u.memory

    if (isDebugEnabled) {
      log.debug(s"Stack = ${stack.size}")
      stack.l.zipWithIndex.foreach { p =>
        log.debug(f"${p._2}%04d: ${Hex.encode(p._1.toPaddedByteArray)}")
      }
      log.debug(s"Memory = ${u.activeMemory * WordSize}")
      0L until (u.activeMemory.toLong * WordSize / 16) foreach { i =>
        val start = i * 16
        val end = start + 16
        val content = start.until(end).map(p => u.memory.load(p)).map(b => f"$b%02x").mkString(" ")
        log.debug(f"$start%04d: $content")
      }

      log.debug(f"PC ${u.pc}%08d: ${OpCodeMaps.Value2Name(w)}")
    }

    val builder = new ExecResultBuilder(theta, u, a, i)
      .setPc(getNextInstruction(w, pc))

    val binaryArithmetic = (op: (Word, Word) => Word, cost: UInt256) => {
      val (x, y, stack1) = stack.pop2()
      builder.setCost(cost).burnGas().setStack(stack1.push(op(x, y)))
    }
    val relational = (op: (Word, Word) => Boolean, cost: UInt256) => {
      val (x, y, stack1) = stack.pop2()
      builder.setCost(cost).burnGas().setStack(stack1.push(op(x, y)))
    }
    val unary = (op: Word => Word, cost: UInt256) => {
      val (x, stack1) = stack.pop()
      builder.setCost(cost).burnGas().setStack(stack1.push(op(x)))
    }

    val ternary = (op: (Word, Word, Word) => Word, cost: UInt256) => {
      val (x, y, z, stack1) = stack.pop3()
      builder.setCost(cost).burnGas().setStack(stack1.push(op(x, y, z)))
    }

    val copyToMem = (data: Array[Byte], stack: Stack, base: Int) => {
      val (mPos, iPos, size, stack1) = stack.pop3()
      builder.setStack(stack1)
        .setActiveMemoryBytes(mPos, size)
        .setCost(getCopyCost(size, base))
        .burnGas()
        .setMemory(mem.store(mPos, data.read(iPos, size.toInt)))
    }

    val push = (cost: UInt256, v: UInt256) => {
      builder.setCost(cost).burnGas().setStack(stack.push(v))
    }

    val ret: ExecResultBuilder = w match {
      case OpCodes.ADD => {
        binaryArithmetic(_ + _, Fee.GAS_VERYLOW)
      }
      case OpCodes.MUL => {
        binaryArithmetic(_ * _, Fee.GAS_LOW)
      }
      case OpCodes.SUB => {
        binaryArithmetic(_ - _, Fee.GAS_VERYLOW)
      }
      case OpCodes.DIV => {
        binaryArithmetic(_ / _, Fee.GAS_LOW)
      }
      case OpCodes.SDIV => {
        binaryArithmetic(_ sdiv _, Fee.GAS_LOW)
      }
      case OpCodes.MOD => {
        binaryArithmetic(_ % _, Fee.GAS_LOW)
      }
      case OpCodes.SMOD => {
        binaryArithmetic(_ smod _, Fee.GAS_LOW)
      }
      case OpCodes.ADDMOD => {
        ternary((x, y, z) => x.addMod(y, z), Fee.GAS_MID)
      }
      case OpCodes.MULMOD => {
        ternary((x, y, z) => x.mulMod(y, z), Fee.GAS_MID)
      }
      case OpCodes.EXP => {
        val (x, y, stack1) = stack.pop2()
        val cost = if (y == UInt256(0)) {
          Fee.GAS_EXP
        } else {
          Fee.GAS_EXP + Fee.GAS_EXPBYTE * y.byteLength
        }
        binaryArithmetic(_ pow _, cost)
      }
      case OpCodes.SIGNEXTEND => {
        binaryArithmetic((x ,y) => {
          if (x < WordSize) {
            val testbit = x.toInt * 8 + 7
            if (y.v.testBit(testbit)) {
              y | (UInt256.Max + 1 - BigInteger.ONE.shiftLeft(testbit))
            } else {
              y & (BigInteger.ONE.shiftLeft(testbit).subtract(BigInteger.ONE))
            }
          } else {
            y
          }
        }, Fee.GAS_LOW)
      }

      case OpCodes.LT => {
        relational(_ < _, Fee.GAS_VERYLOW)
      }
      case OpCodes.GT => {
        relational(_ > _, Fee.GAS_VERYLOW)
      }
      case OpCodes.SLT => {
        relational(_ slt _, Fee.GAS_VERYLOW)
      }
      case OpCodes.SGT => {
        relational(_ sgt _, Fee.GAS_VERYLOW)
      }
      case OpCodes.EQ => {
        relational(_ == _, Fee.GAS_VERYLOW)
      }
      case OpCodes.ISZERO => {
        unary(_ == UInt256.Zero, Fee.GAS_VERYLOW)
      }
      case OpCodes.AND => {
        binaryArithmetic(_ & _, Fee.GAS_VERYLOW)
      }
      case OpCodes.OR => {
        binaryArithmetic(_ | _, Fee.GAS_VERYLOW)
      }
      case OpCodes.XOR => {
        binaryArithmetic(_ ^ _, Fee.GAS_VERYLOW)
      }
      case OpCodes.NOT => {
        unary(~_, Fee.GAS_VERYLOW)
      }
      case OpCodes.BYTE => {
        // get n-th byte of x
        binaryArithmetic(
          (n, x) => if (n < 32) java.lang.Byte.toUnsignedInt(x.toPaddedByteArray(n.toInt)) else 0,
          Fee.GAS_VERYLOW)
      }

      case OpCodes.SHA3 => {
        val (pos, size, stack1) = stack.pop2()
        val cost = MathUtils.divCeiling(size, UInt256(32))
          .mulChecked(Fee.GAS_SHA3WORD)
          .addChecked(Fee.GAS_SHA3)
        builder.setCost(cost)
          .setActiveMemoryBytes(pos, size)
          .burnGas()
          .setStack(stack1.push(Kec.hash(mem.load(pos, size))))
      }

      case OpCodes.ADDRESS => {
        push(Fee.GAS_BASE, i.address.toUInt256)
      }
      case OpCodes.BALANCE => {
        val (x, stack1) = stack.pop()
        unary(x => {
          val addr = Address(x)
          theta.get(addr) match {
            case Some(acc) => acc.balance
            case _ => UInt256.Zero
          }
        }, Fee.GAS_BALANCE)
      }
      case OpCodes.ORIGIN => {
        push(Fee.GAS_BASE, i.origin.toUInt256)
      }
      case OpCodes.CALLER => {
        push(Fee.GAS_BASE, i.caller.toUInt256)
      }
      case OpCodes.CALLVALUE => {
        push(Fee.GAS_BASE, i.value)
      }
      case OpCodes.CALLDATALOAD => {
        unary(x => i.data.read(x, WordSize), Fee.GAS_VERYLOW)
      }
      case OpCodes.CALLDATASIZE => {
        push(Fee.GAS_BASE, i.data.size)
      }
      case OpCodes.CALLDATACOPY => {
        copyToMem(i.data, stack, Fee.GAS_VERYLOW)
      }
      case OpCodes.CODESIZE => {
        push(Fee.GAS_BASE, i.code.size)
      }
      case OpCodes.CODECOPY => {
        copyToMem(i.code, stack, Fee.GAS_VERYLOW)
      }
      case OpCodes.GASPRICE => {
        push(Fee.GAS_BASE, i.gasPrice)
      }
      case OpCodes.EXTCODESIZE => {
        unary(addr => {
          theta.get(Address(addr)) match {
            case None => Array[Byte]()
            case Some(acc) => db.get(acc.codeHash).size
          }
        }, Fee.GAS_EXTCODE)
      }
      case OpCodes.EXTCODECOPY => {
        val (addr, stack1) = stack.pop()
        val code = theta.get(Address(addr)) match {
          case None => Array[Byte]()
          case Some(acc) => db.get(acc.codeHash)
        }
        copyToMem(code, stack1, Fee.GAS_EXTCODE)
      }
      case OpCodes.RETURNDATASIZE => {
        push(Fee.GAS_BASE, u.output.size)
      }
      case OpCodes.RETURNDATACOPY => {
        copyToMem(u.output, stack, Fee.GAS_VERYLOW)
      }

      case OpCodes.BLOCKHASH => {
        unary(n => blockContext.getBlockHash(blockContext.getCurrentHeader, n), Fee.GAS_BLOCKHASH)
      }
      case OpCodes.COINBASE => {
        push(Fee.GAS_BASE, blockContext.getCurrentHeader.benificiary.toUInt256)
      }
      case OpCodes.TIMESTAMP => {
        push(Fee.GAS_BASE, blockContext.getCurrentHeader.timestamp)
      }
      case OpCodes.NUMBER => {
        push(Fee.GAS_BASE, blockContext.getCurrentHeader.number)
      }
      case OpCodes.DIFFICULTY => {
        push(Fee.GAS_BASE, blockContext.getCurrentHeader.difficulty)
      }
      case OpCodes.GASLIMIT => {
        push(Fee.GAS_BASE, blockContext.getCurrentHeader.gasLimit)
      }

      case OpCodes.POP => {
        val (_, s) = stack.pop()
        builder.setCost(Fee.GAS_BASE).burnGas().setStack(s)
      }
      case OpCodes.MLOAD => {
        val (x, stack1) = stack.pop()
        builder.setActiveMemoryBytes(x.toLong + WordSize - 1)
          .setCost(Fee.GAS_VERYLOW)
          .burnGas()
          .setStack(stack1.push(mem.load(x, WordSize)))
      }
      case OpCodes.MSTORE => {
        val (x, y, stack1) = stack.pop2()
        builder.setActiveMemoryBytes(x.toLong + WordSize - 1)
          .setCost(Fee.GAS_VERYLOW)
          .burnGas()
          .setStack(stack1)
          .setMemory(mem.store(x, y))
      }
      case OpCodes.MSTORE8 => {
        val (x, y, stack1) = stack.pop2()
        builder.setActiveMemoryBytes(x.toLong)
          .setCost(Fee.GAS_VERYLOW)
          .burnGas()
          .setStack(stack1)
          .setMemory(mem.store(x, y.toByte))
      }
      case OpCodes.SLOAD => {
        val (key, stack1) = stack.pop()
        builder.setCost(Fee.GAS_SLOAD)
          .burnGas()
          .setStack(stack1.push(getStorage(theta, i.address, key)))
      }
      case OpCodes.SSTORE => {
        val (k, v, stack1) = stack.pop2()
        val oldValue = getStorage(theta, i.address, k)
        val cost = if (v != UInt256.Zero && oldValue == UInt256.Zero) {
          Fee.GAS_SSET
        } else {
          Fee.GAS_SRESET
        }
        val refund = if (v == UInt256.Zero && oldValue != UInt256.Zero) {
          Fee.REFUND_SCLEAR
        } else {
          0
        }
        builder.setCost(cost)
          .burnGas()
          .setTheta(setStorage(theta, i.address, k, v))
          .setStack(stack1)
          .refundGas(refund)
      }
      case OpCodes.JUMP => {
        val (x, stack1) = stack.pop()
        val dest = x.toInt
        if (x > i.code.size || !aux.jumpDests.contains(dest)) {
          throw ExecutionException("Invalid jump dest")
        }
        builder.setCost(Fee.GAS_MID)
          .burnGas()
          .setStack(stack1)
          .setPc(dest)
      }
      case OpCodes.JUMPI => {
        val (x, y, stack1) = stack.pop2()
        val jump = if (y != UInt256(0)) {
          val dest = x.toInt
          if (x> i.code.size || !aux.jumpDests.contains(dest)) {
            throw ExecutionException("Invalid jump dest")
          }
          builder.setPc(dest)
        } else {
          builder
        }
        jump.setCost(Fee.GAS_HIGH).burnGas().setStack(stack1)
      }
      case OpCodes.PC => {
        push(Fee.GAS_BASE, pc)
      }
      case OpCodes.MSIZE => {
        push(Fee.GAS_BASE, 32 * u.activeMemory)
      }
      case OpCodes.GAS => {
        push(Fee.GAS_BASE, u.gas - Fee.GAS_BASE)
      }
      case OpCodes.JUMPDEST => {
        // NO-OP
        builder.setCost(Fee.GAS_JUMPDEST).burnGas()
      }

      case _ if w >= OpCodes.PUSH1 && w <= OpCodes.PUSH32 => {
        val n = w - OpCodes.PUSH1 + 1
        val x = UInt256(i.code.read(pc + 1, n))
        push(Fee.GAS_VERYLOW, x)
      }

      case dup if dup >= OpCodes.DUP1 && dup <= OpCodes.DUP16 => {
        push(Fee.GAS_VERYLOW, stack(dup - OpCodes.DUP1))
      }

      case swap if swap >= OpCodes.SWAP1 && swap <= OpCodes.SWAP16 => {
        builder.setCost(Fee.GAS_VERYLOW).burnGas().setStack(stack.swap(swap - OpCodes.SWAP1 + 1))
      }

      case _ if w >= OpCodes.LOG0 && w <= OpCodes.LOG4 => {
        val (pos, len, stack1) = stack.pop2()
        val data = mem.load(pos, len)
        val numTopics = w - OpCodes.LOG0
        val cost = len.mulChecked(Fee.GAS_LOGDATA)
          .addChecked(Fee.GAS_LOG + Fee.GAS_LOGTOPIC * numTopics)
        builder.setCost(cost)
          .setActiveMemoryBytes(pos, len)
          .burnGas()
        val (stack2, topics) = ((stack1, Vector[Word]()) /: (0 until numTopics)) { case (p, _) =>
          val (s, t) = p
          val (x, s2) = s.pop()
          (s2, t :+ x)
        }
        builder.setStack(stack2)
          .addLog(LogEntry(i.address, topics, data))
      }

      case OpCodes.CREATE => {
        val (value, codePos, codeSize, stack1) = stack.pop3()
        val code = mem.load(codePos, codeSize)
        val acc = theta(i.address)
        builder.setCost(Fee.GAS_CREATE).setActiveMemoryBytes(codePos, codeSize)
        val cost = builder.getTotalCost()
        val gasLeft = if (builder.gas > cost) builder.gas - cost else UInt256(0)
        val createGas = getCreateGas(gasLeft)
        builder.setCost(Fee.GAS_CREATE + createGas).burnGas()

        if (acc.balance >= value && i.depth < 1024) {
          val newAddr = Account.generateAddress(i.address, acc.nonce)
          val ret = create(
            theta,
            i.address,
            i.origin,
            newAddr,
            createGas,
            i.gasPrice,
            value,
            code,
            i.depth + 1,
            i.perm
          )
          val x = if (ret.hasError) UInt256(0) else newAddr.toUInt256
          val theta2 = if (ret.hasError) theta else ret.theta
          builder
            .refundGas(ret.gasLeft)
            .setStack(stack1.push(x))
            .setTheta(theta2)
            .setSubstate(a.merge(ret.substate))
        } else {
          log.debug("create failed")
          builder.refundGas(createGas).setStack(stack1.push(0))
        }
      }
      case OpCodes.CALL => {
        val (gas, to, value, stack1) = stack.pop3()
        val toAccount = Address(to)
        callMessage(builder.setStack(stack1), gas, toAccount, toAccount, value, value, i.perm)
      }
      case OpCodes.CALLCODE => {
        val (gas, code, value, stack1) = stack.pop3()
        val codeAccount = Address(code)
        callMessage(builder.setStack(stack1), gas, i.address, codeAccount, value, value, i.perm)
      }
      case OpCodes.DELEGATECALL => {
        val (gas, code, stack1) = stack.pop2()
        val codeAccount = Address(code)
        callMessage(builder.setStack(stack1), gas, i.address, codeAccount, 0, i.value, i.perm)
      }
      case OpCodes.STATICCALL => {
        val (gas, to, stack1) = stack.pop2()
        val toAccount = Address(to)
        callMessage(builder.setStack(stack1), gas, toAccount, toAccount, 0, 0, false)
      }
      case OpCodes.SELFDESTRUCT => {
        val (r, stack1) = stack.pop()
        val rAddr = Address(r)
        val cost = Fee.GAS_SELFDESTRUCT
          + (if (isDeadAccount(theta, rAddr)) Fee.GAS_NEWACCOUNT else 0)
        builder.setCost(cost).burnGas()

        val acc = theta(i.address)
        val rAcc = theta.getOrElse(rAddr, null)
        val rNewAcc = if (rAcc == null && acc.balance == UInt256.Zero) {
          rAcc
        } else if (rAddr != i.address) {
          if (rAcc == null) {
            Account().copy(balance = acc.balance)
          } else {
            rAcc.copy(balance = rAcc.balance + acc.balance)
          }
        } else {
          rAcc.copy(balance = 0)
        }
        val theta2 = {
          val tmp = theta + (i.address -> acc.copy(balance = 0))
          if (rNewAcc == null) {
            tmp
          } else {
            tmp + (rAddr -> rNewAcc)
          }
        }
        val a2 = a.copy(suisides = a.suisides :+ i.address)
        builder.setStack(stack1)
          .setSubstate(a2)
          .setTheta(theta2)
          .setOutput(Array())
      }

      case OpCodes.RETURN | OpCodes.REVERT => {
        val (addr, len, _) = stack.pop2()
        builder.setActiveMemoryBytes(addr, len)
          .setCost(Fee.GAS_ZERO)
          .burnGas()
          .setOutput(u.memory.load(addr, len))
      }

      case OpCodes.STOP => {
        builder.setCost(Fee.GAS_ZERO).burnGas().setOutput(Array())
      }

      case _ => {
        if (OpCodeMaps.Value2Name.contains(w)) {
          throw ExecutionException(s"Unhandled op: ${OpCodeMaps.Value2Name(w)}")
        } else {
          throw ExecutionException(s"Invalid op: $w")
        }
      }
    }

    if (ret.cost != UInt256(0) && ret.gas == u.gas) {
      throw ExecutionException("Gas not burned")
    }

    if (isDebugEnabled) {
      log.debug(s"GAS: ${ret.gas}, COST: ${u.gas - ret.gas}")
      log.debug("")
    }
    ret.build
  }

  private def getStorage(theta: SystemState, addr: Address, key: UInt256): UInt256 = {
    val acc = theta(addr)
    val storage = new Trie(db)
    UInt256(storage.get(acc.storageRoot, key.toPaddedByteArray))
  }

  private def setStorage(
      theta: SystemState, addr: Address, key: UInt256, value: UInt256): SystemState = {
    val acc = theta(addr)
    val storage = new Trie(db)
    val newRoot = if (value == UInt256.Zero) {
      storage.delete(acc.storageRoot, key.toPaddedByteArray)
    } else {
      storage.update(acc.storageRoot, key.toPaddedByteArray, value.toPaddedByteArray)
    }
    theta + (addr -> acc.copy(storageRoot = Hex.encode(newRoot)))
  }

  private def callMessage(
      builder: ExecResultBuilder, gas: UInt256, recipient: Address, codeAccount: Address,
      transferValue: UInt256, value: UInt256, perm: Boolean): ExecResultBuilder = {
    val stack = builder.stack
    val mem = builder.memory
    val i = builder.i
    val theta = builder.theta
    val a = builder.a

    val (inOffset, inSize, stack1) = stack.pop2()
    val (outOffset, outSize, stack2) = stack1.pop2()
    val data = mem.load(inOffset, inSize)
    val balance = theta(i.address).balance
    val costNew = if (needCreateAccount(theta, recipient, transferValue)) {
      Fee.GAS_NEWACCOUNT
    } else {
      0
    }
    val costXfer = if (transferValue > 0) {
      Fee.GAS_CALLVALUE
    } else {
      0
    }
    val costExtra = callFee + costXfer + costNew
    val costGasCap = getCallGasCap(builder.gas, gas, costExtra)
    val callGas = costGasCap + (if (transferValue > 0) Fee.GAS_CALLSTIPEND else 0)
    val cost = costGasCap + costExtra
    builder.setActiveMemoryBytes(inOffset, inSize)
      .setActiveMemoryBytes(outOffset, outSize)
      .setCost(cost)
      .burnGas()

    if (balance >= transferValue && i.depth < 1024) {
      val ret = callMessage(
        theta, i.address, i.origin, recipient, codeAccount,
        callGas, i.gasPrice, transferValue, value,
        data, i.depth + 1, perm)
      val x = if (ret.hasError) 0 else 1
      val theta2 = if (ret.hasError) theta else ret.theta
      val output = if (ret.output == null) {
        Array[Byte]()
      } else {
        val size = if (outSize < ret.output.size) outSize.toInt else ret.output.size
        ret.output.read(0, size)
      }
      builder
        .refundGas(ret.gasLeft)
        .setStack(stack2.push(x))
        .setTheta(theta2)
        .setMemory(mem.store(outOffset, output))
        .setSubstate(a.merge(ret.substate))
    } else {
      log.debug("Call failed")
      builder.refundGas(callGas)
        .setStack(stack2.push(0))
    }
  }

  def getCallGasCap(gasAvail: UInt256, gasRequired: UInt256, costExtra: UInt256): UInt256 = {
    if (gasAvail >= costExtra) {
      val tmp = gasAvail - costExtra
      val cap = tmp - tmp / 64
      if (cap < gasRequired) cap else gasRequired
    } else {
      gasRequired
    }
  }

  def callFee = Fee.GAS_CALL_EIP150

  def getCreateGas(gas: UInt256) = gas - gas / 64

  def needCreateAccount(theta: SystemState, addr: Address, transferValue: UInt256) =
    isDeadAccount(theta, addr) && transferValue > 0
}
