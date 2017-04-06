package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection of a set with the empty set should be empty") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val e = empty
      assert(intersect(s, e)(1) === e(1), "Empty set intersection")
      assert(intersect(s, e)(2) === e(2), "Empty set intersection")
      assert(intersect(s, e)(3) === e(3), "Empty set intersection")
    }
  }

  test("filter should find all elements satisfying predicate") {
    val nums = Range(0,10)
    val evens = Range(0,10,2)
    val odds = Range(1,10,2)

    val s    = (a: Int) => nums  contains a
    val even = (a: Int) => evens contains a
    val odd  = (a: Int) => odds  contains a

    val isEven = (a: Int) => a % 2 == 0
    val isOdd = (a: Int) => a % 2 != 0

    for (num <- nums) {
      assert(filter(s, isEven)(num) === even(num), "Even")
      assert(filter(s, isOdd)(num) === odd(num), "Odd")
    }
  }

  test("union of nonempty set and empty set should be nonempty set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val nums = Range(1,4)

      for (num <- nums) {
        assert(union(s, empty)(num) === s(num))
      }
    }
  }

  test("intersection should be associative and commutative") {
    val data1 = List(0,1,2,3,4,5,6,7,8,9,10)
    val data2 = List(0,2,4,6,8,10,11,13)
    val data3 = List(1,3,5,7,9,15,21)
    val s1 = (a: Int) => data1.contains(a)
    val s2 = (a: Int) => data2.contains(a)
    val s3 = (a: Int) => data3.contains(a)

    for (v <- data1) {
      assert(intersect(intersect(s1, s2), s3)(v) === intersect(s1, intersect(s2, s3))(v))
      assert(intersect(s1, s2)(v) === intersect(s2, s1)(v))
      assert(intersect(s1, s3)(v) === intersect(s3, s1)(v))
    }
  }

  test("intersection should contain only elements common to each set") {
    val data1 = List(0,1,2,3,4,5,6,7,8,9,10)
    val data2 = List(0,2,4,6,8,10,11,13)
    val s1 = (a: Int) => data1.contains(a)
    val s2 = (a: Int) => data2.contains(a)

    // Calculate the elements in set 1 not in set 2
    val diff12 = diff(s1,s2)
    // Calculate the elements in set 2 not in set 1
    val diff21 = diff(s2, s1)
    val intersect12 = intersect(s1,s2)
    // The intersection of set 1 with set 2 should contain no elements
    // in the differences between them.
    for (v <- data1) {
      assert(intersect(intersect12, diff12)(v) === intersect(intersect12, diff21)(v))
      assert(intersect(intersect12, diff12)(v) === empty(v))
      assert(intersect(intersect12, diff21)(v) === empty(v))
    }
  }

  test("universal quantification and existential quantification") {
    val data = Range(-bound, bound)
    val s = (a: Int) => data contains a
    val p = (a: Int) => (a > -(bound+1)) && (a < bound+1)

    assert(forall(s, p))
    assert(exists(s, p))
  }

  test("exists: {1,2,3,4}") {
    val data = List(1,2,3,4)
    val s = (a: Int) => data contains a

    assert(exists(s, (a: Int) => a == 2))
  }

  test("forall: {1,2,3,4,5,1000}") {
    val data = List(1,2,3,4,5,1000)
    val s = (a: Int) => data contains a
    val p = (a: Int) => a < 5

    assert(!forall(s, p))
    assert(exists(s, !p(_)))
  }

  test("map (_+1) {1,2,3,4,5,6,8,8,9,1000} == {2,3,4,5,6,7,8,9,10,1001}") {
    val g = List(1,2,3,4,5,6,7,8,9,1000)
    val e = g map ((x: Int) => x+1)

    val given = (a: Int) => g contains a
    val expected = (a: Int) => e contains a

    for (v <- e) {
      assert(map(given, _+1)(v) === expected(v))
    }
  }
}
