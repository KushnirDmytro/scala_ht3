/**
  * Created by d1md1m on 25.05.17.
  */


object rationalFancyOperators {

  class Rational(n: Int, d: Int) {
    require(d != 0, "Zero denominator is not allowed!")

    private def gcd(a: Int, b: Int): Int = {
      if (b == 0) a else gcd(b, a % b)
    }

    private val isPositive =
    (n >= 0 && d >= 0) || (n < 0 && d < 0)

    private def abs(a:Int):Int =
    if (a >= 0) a else -a

    val numer: Int = n
    val denom: Int = d

    //opers
    def +(that: Rational): Rational =
      new Rational(this.numer * that.denom + this.denom * that.numer,
        this.denom * that.denom)


    def unary_- = new Rational(-numer, denom)

    def -(that: Rational): Rational =
      this + - that

    def *(that: Rational): Rational =
      new Rational(this.numer * that.numer,
        this.denom * that.denom)

    def /(that: Rational): Rational =
      new Rational(this.numer * that.denom,
        this.denom * that.numer)

    def neg(): Rational =
      new Rational(-this.numer, this.denom)

    def ==(that: Rational): Boolean = {
      this.numer * that.denom == this.denom * that.numer
    }

    def < (that: Rational): Rational = {
      if (this.numer / this.denom < that.numer / that.denom)
        this
      else
        that
    }


    private def realNumer =
      if ((isPositive && numer >=0) || (!isPositive && numer <0))
        numer / abs(gcd(n,d))
      else -numer / abs(gcd(n,d))

    override def toString: String =  realNumer + "/" + abs(denom / gcd(n,d))
  }


  //def toStr(r:Rational) = numer(r) + "/" + denom(r)






  def main(args: Array[String]): Unit = {
    val one_half = new Rational(1, 2)
    val one_third = new Rational(1, 3)
    var rez_add = one_third + one_third
    println(rez_add)
    rez_add == new Rational(2, 3)

    var rez_sub = one_half - one_third
    println(rez_sub)

    var rez_mul = one_half * one_third
    println(rez_mul)

    var rez_div = one_half / one_third
    println(rez_div)

    var check = one_half - one_half - one_third
    println(check)

    var neg_one_half = -one_half
    println(neg_one_half)

    println(one_half == new Rational(5,10))
/*
    println(new Rational(1,0))

    var test_wierd = new Rational(1,0)
    println(test_wierd + test_wierd)
    */
    println(one_half + one_third)

    println(new Rational(1, -7))

  }
}
