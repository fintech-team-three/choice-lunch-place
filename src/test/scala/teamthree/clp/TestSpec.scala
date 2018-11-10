package teamthree.clp

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class TestSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new mutable.Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }
}
