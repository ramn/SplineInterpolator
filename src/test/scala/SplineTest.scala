package se.ramn.battery

import org.scalatest.FunSuite


class SplineTest extends FunSuite {

  def assertWithinErrorMargin(
    expected: Double,
    actual: Double,
    epsilon: Double
  ) {
    assert(actual >= expected - epsilon && actual <= expected + epsilon)
  }

  test("polynominal") {
    implicit class PowableInt(d: Int) {
      def ^^(exponent: Int): Double = math.pow(d, exponent)
    }
    implicit class PowableDouble(d: Double) {
      def ^^(exponent: Int): Double = math.pow(d, exponent)
    }
    val polyn = Polynomial(2, 3, 4)
    val expected = (x: Int) => {
      2 * (x ^^ 0) + 3 * (x ^^ 1) + 4 * (x ^^ 2)
    }
    val x = 3
    expectResult(expected(x)) {
      polyn.evaluate(x)
    }
  }

  test("interpolate 3 data points") {
    val xs = Array(1.0, 2.0, 3.0)
    val ys = Array(2.0, 8.0, 3.0)
    val splineFunction = Interpolator.splineInterpolate(xs, ys)
    val epsilon = 0.01
    assertWithinErrorMargin(
      expected=8.0,
      actual=splineFunction(1.99999),
      epsilon=epsilon)
    assertWithinErrorMargin(
      expected=3.0,
      actual=splineFunction(3.0),
      epsilon=epsilon)
  }

  test("interpolate 5 data points") {
    val xs = Array(1.0, 2.0, 3.0, 5.0, 6.0)
    val ys = Array(1.0, 2.0, 4.0, 16.0, 32.0)
    val splineFunction = Interpolator.splineInterpolate(xs, ys)
    val epsilon = 0.01
    assertWithinErrorMargin(
      expected=4.0,
      actual=splineFunction(3),
      epsilon=epsilon)
    assertWithinErrorMargin(
      expected=4.0,
      actual=splineFunction(2.9999),
      epsilon=epsilon)
    assertWithinErrorMargin(
      expected=1.0,
      actual=splineFunction(1.0),
      epsilon=epsilon)
  }

  test("Polynomial builtin testcase") {
      val zero = new Polynomial(0, 0)

      val p1   = new Polynomial(4, 3)
      val p2   = new Polynomial(3, 2)
      val p3   = new Polynomial(1, 0)
      val p4   = new Polynomial(2, 1)
      val p    = p1.plus(p2).plus(p3).plus(p4)    // 4x^3 + 3x^2 + 1

      val q1   = new Polynomial(3, 2)
      val q2   = new Polynomial(5, 0)
      val q    = q1.plus(q2)                      // 3x^2 + 5

      assert(q1.evaluate(1.0) === 3)
      assert(q2.evaluate(8.0) === 5)
      assert(q.evaluate(1.0) === 8.0)


      val r    = p.plus(q)
      val s    = p.times(q)
      val t    = p.compose(q)

      //println("zero(x) =     " + zero)
      //println("p(x) =        " + p)
      //println("q(x) =        " + q)
      //println("p(x) + q(x) = " + r)
      //println("p(x) * q(x) = " + s)
      //println("p(q(x))     = " + t)
      //println("0 - p(x)    = " + zero.minus(p))
      //println("p(3)        = " + p.evaluate(3))
      //println("p'(x)       = " + p.differentiate)
      //println("p''(x)      = " + p.differentiate.differentiate)
  }
}

