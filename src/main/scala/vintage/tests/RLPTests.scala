package vintage.tests

import java.math.BigInteger
import spray.json._
import vintage.Hex
import vintage.rlp._

object RLPTests {
  def check(expected: JsValue, actual: RLPObject): Boolean = {
    actual match {
      case RLPBytes(b: Array[Byte]) => {
        expected match {
          case JsString(s) if s.startsWith("#") => {
            val num = new BigInteger(s.substring(1))
            val num2 = new BigInteger(1, b)
            if (num == num2) {
              true
            } else {
              println(s"Mismatch: $num expected, got $num2")
              false
            }
          }
          case JsString(s) if !s.startsWith("#") => {
            val s2 = new String(b)
            if (s == s2) {
              true
            } else {
              println(s"Mismatch: $s expected, got $s2")
              false
            }
          }
          case JsNumber(n) => {
            val l = n.longValue
            val l2 = (new BigInteger(1, b)).longValue
            if (l == l2) {
              true
            } else {
              println(s"Mismatch: $l expected, got $l2")
              false
            }
          }
          case _ => false
        }
      }
      case RLPList(l) => {
        expected match {
          case JsArray(a) => {
            if (l.size != a.size) {
              println(s"List size mismatch: ${a.size} expected, got ${l.size}")
              false
            } else if (a.zip(l).forall { case (a, b) => check(a, b) }) {
              true
            } else {
              false
            }
          }
          case _ => false
        }
      }
    }
  }

  def test(testCase: JsObject): Boolean = {
    val inputs = testCase.fields
    val in = inputs("in")
    val out = inputs("out") match {
      case JsString(s) => s
      case _ => ???
    }

    try {
      val decoded = RLPCodec.decode(Hex.decode(out))
      if (in == JsString("INVALID")) {
        false
      } else if (in != JsString("VALID") && !check(in, decoded)) {
        false
      } else {
        true
      }
    } catch {
      case e: RuntimeException => in == JsString("INVALID")
    }
  }

  def main(args: Array[String]): Unit = {
    FileUtils.testFile("fixtures/RLPTests/rlptest.json", test)
    FileUtils.testFile("fixtures/RLPTests/invalidRLPTest.json", test)
  }
}
