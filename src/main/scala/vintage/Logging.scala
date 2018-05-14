package vintage

import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

trait Logging {
  protected[vintage] lazy val rawLog = LoggerFactory.getLogger(getClass)
  protected[vintage] lazy val log = Logger(rawLog)

  def isDebugEnabled: Boolean = rawLog.isDebugEnabled
}
