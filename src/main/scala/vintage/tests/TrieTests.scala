package vintage.tests

import spray.json._
import vintage._

object TrieTests {
  def test(testCase: JsObject): Boolean = {
    val decodeString =
      (s: String) => if (s.startsWith("0x")) Hex.decode(s.substring(2)) else s.getBytes
    val in = testCase.fields("in").asInstanceOf[JsArray].elements.map { kv =>
      val k = kv.asInstanceOf[JsArray].elements(0).asInstanceOf[JsString].value
      val v = kv.asInstanceOf[JsArray].elements(1)
      val key = decodeString(k)
      val value = v match {
        case JsString(s) => decodeString(s)
        case JsNull => null
        case _ => ???
      }
      (key, value)
    }
    // val root = testCase.fields("root") match {
    //   case JsString(s) => s
    //   case _ => ???
    // }
    val db = HashMapDb()
    val trie = new Trie(db)
    val actualRoot = (Trie.Empty /: in) { case (root, kv) =>
      if (kv._2 == null) {
        trie.delete(root, kv._1)
      } else {
        trie.update(root, kv._1, kv._2)
      }
    }
    in.map { case (k, v) => (Hex.encode(k), v) }.toMap.forall { case (k, v) =>
      val expected = if (v == null) "" else Hex.encode(v)
      val actual = Hex.encode(trie.get(actualRoot, Hex.decode(k)))
      if (actual != expected) {
        println(s"Expect $expected, got $actual")
        false
      } else {
        true
      }
    }
    // Hex.encode(actualRoot) == root.substring(2)
  }

  def main(args: Array[String]): Unit = {
    FileUtils.testFile("fixtures/TrieTests/trietest.json", test)
    FileUtils.testFile("fixtures/TrieTests/trietest_secureTrie.json", test)
  }
}
