package vintage.tests

import java.io.File
import scala.io.Source
import spray.json._
import vintage.Logging

case class TestException(msg: String) extends RuntimeException(msg)

object FileUtils extends Logging {
  def collectTests(dir: String): Array[String] = collectTests(new File(dir)).map(_.getPath)

  def collectTests(f: File): Array[File] = {
    val entries = f.listFiles
    val files = entries.filter(e => e.isFile && e.getName.endsWith(".json"))
    files ++ entries.filter(_.isDirectory).flatMap(d => collectTests(d))
  }

  def loadJson(filename: String): JsValue = {
    val src = Source.fromFile(filename)
    val json = JsonParser(src.mkString)
    src.close()
    json
  }

  def testFile(filename: String, test: JsObject => Boolean): Boolean = {
    log.info("--------")
    log.info(s"Testing $filename ...")
    var failures = List[String]()
    val tests = loadJson(filename).asJsObject.fields
    tests.foreach { case (name, v) =>
      log.debug(s"Testing $filename:$name")
      if (!test(v.asJsObject)) {
        failures = name::failures
      }
    }
    if (failures.isEmpty) {
      true
    } else {
      log.error(s"$filename failures: ${failures.size} / ${tests.size}")
      failures.foreach(log.error(_))
      false
    }
  }

  def testDir(dir: String, test: JsObject => Boolean): Unit = {
    val tests = collectTests(dir)
    val failed = tests.filterNot(testFile(_, test))
    if (!failed.isEmpty) {
      log.error(s"$dir: ${failed.size}/${tests.size} failed")
      throw TestException("testDir failed")
    }
  }
}
