package vintage.tests

import spray.json._
import vintage._
import vintage.rlp._

object TransactionTests extends Logging {
  def test(testCase: JsObject): Boolean = {
    val rlp = testCase.fields("rlp").asInstanceOf[JsString].value
    val expected = testCase.fields("Byzantium").asJsObject
    try {
      val transaction = RLPCodec.decodeAuto[Transaction](Hex.decode(rlp.substring(2)))
      val sender = transaction.recoverSender()
      if (sender.value == expected.fields("sender").asInstanceOf[JsString].value) {
        true
      } else {
        false
      }
    } catch {
      case e: RuntimeException => {
        if (expected.fields.contains("sender")) {
          log.error("Test failed", e)
          false
        } else {
          true
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    FileUtils.testDir("fixtures/TransactionTests", test)
    // FileUtils.testFile("fixtures/TransactionTests/ttRSValue/TransactionWithSvalue1.json", test)
  }
}
