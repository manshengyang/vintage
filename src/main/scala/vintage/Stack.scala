package vintage

object Stack {
  val Size = 1024
}

case class Stack(val l: List[Word] = List()) extends AnyVal {
  def pop(): (Word, Stack) = l match {
    case x::xs => (x, Stack(xs))
    case _ => throw ExecutionException("Stack underflow")
  }

  def pop2(): (Word, Word, Stack) = l match {
    case x1::x2::xs => (x1, x2, Stack(xs))
    case _ => throw ExecutionException("Stack underflow")
  }

  def pop3(): (Word, Word, Word, Stack) = l match {
    case x1::x2::x3::xs => (x1, x2, x3, Stack(xs))
    case _ => throw ExecutionException("Stack underflow")
  }

  def push(x: Word): Stack = {
    if (l.size == Stack.Size) {
      throw ExecutionException("Stack overflow")
    }
    Stack(x::l)
  }

  def apply(i: Int): Word = if (l.size > i) {
    l(i)
  } else {
    throw ExecutionException("Stack underflow")
  }

  def size(): Int = l.size

  def swap(i: Int): Stack = if (l.size > i) {
    val (l1, l2) = l.splitAt(i)
    Stack((l2.head::l1.tail) ++ (l1.head::l2.tail))
  } else {
    throw ExecutionException("Stack underflow")
  }
}
