package vintage

object MathUtils {
  def divCeiling(x: Int, y: Int) = (x + y - 1) / y
  def divCeiling(x: UInt256, y: UInt256) = UInt256(x.v.add(y.v).subtract(UInt256(1).v).divide(y.v))
}
