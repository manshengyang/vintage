package vintage.tests

import java.math.BigInteger
import spray.json._
import vintage._
import vintage.UInt256Conversions._
import vintage.rlp._

class TestVM(db: Db, blockContext: BlockContext) extends VM(db, blockContext) {
  case class CallCreate(data: Array[Byte], dest: Address, gas: UInt256, value: UInt256)

  var callCreates = Vector[CallCreate]()

  override def callMessage(
      theta: SystemState, sender: Address, origin: Address, recipient: Address,
      codeAccount: Address, gas: UInt256, gasPrice: UInt256, value: UInt256, delValue: UInt256,
      data: Array[Byte], depth: Int, perm: Boolean)
      : ExecutionResult = {
    callCreates = callCreates :+ CallCreate(data, recipient, gas, value)
    ExecutionResult(theta, gas, Substate.Empty, Array())
  }

  override def create(
      theta: SystemState, sender: Address, origin: Address, creationAddr: Address,
      gas: UInt256, gasPrice: UInt256, value: UInt256, code: Array[Byte], depth: Int,
      perm: Boolean): ExecutionResult = {
    callCreates = callCreates :+ CallCreate(code, Address.None, gas, value)
    ExecutionResult(theta, gas, Substate.Empty, Array())
  }

  override def getCallGasCap(gasAvail: UInt256, gasRequired: UInt256, costExtra: UInt256) =
    gasRequired

  override def callFee = Fee.GAS_CALL

  override def getCreateGas(gas: UInt256) = gas

  override def needCreateAccount(theta: SystemState, addr: Address, transferValue: UInt256) =
    VM.isDeadAccount(theta, addr)
}

object VMTests extends Logging {
  def parseData(json: JsValue): Array[Byte] = {
    json match {
      case JsNumber(n) => UInt256(n.longValue).toPaddedByteArray.drop(24)
      case JsString(s) if s.startsWith("0x") => Hex.decode(s.substring(2))
      case JsString(s) => {
        val b = new BigInteger(s)
        val bytes = b.toByteArray
        if (bytes(0) == 0) {
          bytes.drop(1)
        } else {
          bytes
        }
      }
      case _ => ???
    }
  }

  def parseNum(json: JsValue): UInt256 = {
    json match {
      case JsNumber(n) => UInt256(n.longValue)
      case JsString(s) if s.startsWith("0x") => UInt256(Hex.decode(s.substring(2)))
      case JsString(s) => {
        val b = new BigInteger(s)
        UInt256(b)
      }
      case _ => ???
    }
  }

  def test(testCase: JsObject): Boolean = {
    val env = testCase.fields("env").asJsObject.fields
    val coinbase = Address(parseData(env("currentCoinbase")))
    val difficulty = parseNum(env("currentDifficulty")).toLong
    val gasLimit = parseNum(env("currentGasLimit")).toLong
    val number = parseNum(env("currentNumber")).toLong
    val timestamp = parseNum(env("currentTimestamp")).toInt
    // val prevHash = parseData(env.getOrElse("previousHash", JsString("0x")))

    val header = BlockHeader(
      null,
      null,
      coinbase,
      null,
      null,
      null,
      difficulty,
      number,
      gasLimit,
      0,
      timestamp,
      null,
      null,
      0,
    )

    val exec = testCase.fields("exec").asJsObject.fields
    val gasAvail = parseNum(exec("gas"))
    val address = Address(parseData(exec("address")))
    val origin = Address(parseData(exec("origin")))
    val caller = Address(parseData(exec("caller")))
    val value = parseNum(exec("value"))
    val data = parseData(exec("data"))
    val code = parseData(exec("code"))
    val gasPrice = parseNum(exec("gasPrice"))
    val i = ExecutionEnv(
      address, origin, gasPrice, data, caller, value, code, 0, true)

    val db = new HashMapDb()
    val blockContext = new BlockContext {
      def getBlockHash(h: BlockHeader, d: UInt256) = {
        if (d >= h.number || d.toLong < h.number - 256) Array[Byte]()
        else Kec.hash(s"$d".getBytes)
      }
      def getCurrentHeader() = header
    }
    val vm = new TestVM(db, blockContext)

    val preState: SystemState = if (testCase.fields.contains("pre")) {
      testCase.fields("pre").asJsObject.fields.map { case (k, v) =>
        val addr = k
        val acc = v.asJsObject.fields
        val code = parseData(acc("code"))
        val storage = new Trie(db)
        val root = (Trie.Empty /: acc("storage").asJsObject.fields) { (root, kv) =>
          val x = parseNum(JsString(kv._1)).toPaddedByteArray
          val y = parseNum(kv._2).toPaddedByteArray
          storage.update(root, x, y)
        }

        (
          Address(addr),
          Account(
            parseNum(acc("nonce")),
            parseNum(acc("balance")),
            Hex.encode(root),
            db.put(code),
          )
        )
      }.toMap
    } else {
      Map[Address, Account]()
    }
    try {
      val ret = vm.executeCode(preState, gasAvail, i)
      if (testCase.fields.contains("gas")) {
        if (ret.hasError) {
          false
        } else {
          val gasLeft = parseNum(testCase.fields("gas"))
          val output = Hex.encode(parseData(testCase.fields("out")))
          val logHash = testCase.fields("logs").asInstanceOf[JsString].value.substring(2)
          val actualOutput = Hex.encode(ret.output)
          val actualLogHash = Hex.encode(Kec.hash(RLPCodec.encodeAuto[LogEntry](ret.substate.logs)))

          val post = testCase.fields("post").asJsObject.fields
          val postKeys = (ret.theta.keys.map(_.toString) ++ post.keys).toSet
          val trie = new Trie(db)
          val matchState = postKeys.forall { k =>
            val actualAcc = ret.theta.getOrElse(Address(k), null)
            val expectedAcc = post.getOrElse(k, null)
            if (expectedAcc != null) {
              val fields = expectedAcc.asJsObject.fields
              val nonce = parseNum(fields("nonce"))
              val balance = parseNum(fields("balance"))
              val codeHash = Hex.encode(Kec.hash(parseData(fields("code"))))
              if (actualAcc != null) {
                if (nonce != actualAcc.nonce || balance != actualAcc.balance
                    || codeHash != actualAcc.codeHash) {
                  false
                } else {
                  val storage = (Map[String, String]() /: fields("storage").asJsObject.fields) {
                    (m, kv) =>
                      val x = Hex.encode(parseNum(JsString(kv._1)).toPaddedByteArray)
                      val y = Hex.encode(parseNum(kv._2).toPaddedByteArray)
                      m + (x -> y)
                  }
                  val actualStorage = trie.dump(actualAcc.storageRoot)
                  storage == actualStorage
                }
              } else {
                Account(nonce, balance, codeHash).isEmpty
              }
            } else {
              false
            }
          }

          val callCreates = testCase.fields("callcreates").asInstanceOf[JsArray].elements
          val matchCallCreates = vm.callCreates.zip(callCreates).forall { case (c, js) =>
            val expected = js.asJsObject.fields
            val data = Hex.encode(parseData(expected("data")))
            val dest = expected("destination").asInstanceOf[JsString].value
            val gasLimit = parseNum(expected("gasLimit"))
            val value = parseNum(expected("value"))
            (dest == c.dest.toString && gasLimit == c.gas && value == c.value
              && data == Hex.encode(c.data))
          } && vm.callCreates.size == callCreates.size

          if (ret.gasLeft != gasLeft) {
            log.error(s"Gas mismatch: ${ret.gasLeft} $gasLeft")
            false
          } else if (actualOutput != output) {
            log.error(s"Output mismatch: $actualOutput $output")
            false
          } else if (logHash != actualLogHash) {
            log.error(s"Log mismatch: $actualLogHash $logHash")
            false
          } else if (!matchState) {
            log.error(s"Post state mismatch")
            false
          } else if (!matchCallCreates) {
            log.error(s"Call/Create mismatch")
            false
          } else {
            true
          }
        }
      } else if (!ret.hasError) {
        log.error(s"Expect execution failure")
        false
      } else {
        true
      }
    } catch {
      case e: RuntimeException => {
        log.error(e.toString, e)
        false
      }
      case e: NotImplementedError => {
        log.error(e.toString, e)
        false
      }
    }
  }

  def main(args: Array[String]): Unit = {
    FileUtils.testDir("fixtures/VMTests/vmArithmeticTest", test)
    FileUtils.testDir("fixtures/VMTests/vmBitwiseLogicOperation", test)
    FileUtils.testDir("fixtures/VMTests/vmBlockInfoTest/", test)
    FileUtils.testDir("fixtures/VMTests/vmEnvironmentalInfo", test)
    FileUtils.testDir("fixtures/VMTests/vmIOandFlowOperations", test)
    FileUtils.testDir("fixtures/VMTests/vmLogTest", test)
    FileUtils.testDir("fixtures/VMTests/vmPushDupSwapTest", test)
    FileUtils.testDir("fixtures/VMTests/vmRandomTest", test)
    FileUtils.testDir("fixtures/VMTests/vmSha3Test", test)
    FileUtils.testDir("fixtures/VMTests/vmSystemOperations", test)
    FileUtils.testDir("fixtures/VMTests/vmTests", test)
  }
}
